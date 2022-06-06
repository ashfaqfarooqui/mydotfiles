--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit
import System.IO (hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout.Tabbed
--hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks -- (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
    -- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Tuple.Extra as TE
import qualified Data.Map as M


--Actions
import XMonad.Actions.TreeSelect
import qualified  XMonad.Actions.Search as S
import qualified  XMonad.Actions.CycleWS as CWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)

import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)

--layouts
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spiral
import XMonad.Layout.SimplestFloat

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

-- utils
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "termite"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = True



-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask


myBorderWidth :: Dimension
myBorderWidth = 1         -- Sets border width for windows

myNormalColor :: String
myNormalColor   = "#292d3e"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#bbc5ff"  -- Border color of focused windows



altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts


myEditor :: String
myEditor = "emacsclient -c -a emacs "  -- Sets emacs as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select


-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
               
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ "> " ++ ws ++ " </action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


---------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = "xft:Mononoki Nerd Font:size=9"
      , bgColor             = "#292d3e"
      , fgColor             = "#d0d0d0"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = dtXPKeymap
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = S.isPrefixOf
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

-- The same config minus the autocomplete feature which is annoying on
-- certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig
      { autoComplete = Nothing
      }

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("p", passPrompt)         -- get passwords (requires 'pass')
             , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]

------------------------------------------------------------------------
-- XPROMPT KEYMAP (emacs-like key bindings)
------------------------------------------------------------------------
dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line fowards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]


------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
-- I am using the Xmonad.Util.EZConfig module which allows keybindings
-- to be written in simpler, emacs-like format.
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    -- Open my preferred terminal and the FISH shell.
        , ("M-<Return>", spawn myTerminal)

    -- Windows
        , ("M-q", kill)                           -- Kill the currently focused client
        

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats"))       -- Toggles my 'floats' layout
        , ("M-<Delete>", withFocused $ windows . W.sink) -- Push floating window back to tile
        , ("M-S-<Delete>", sinkAll)                      -- Push ALL floating windows to tile

    -- Tree Select
        , ("M-t", treeselectA Main.tsDefaultConfig)  -- tree select actions menu

    -- Windows navigation
        , ("M-m", windows W.focusMaster)     -- Move focus to the master window
        , ("M-j", windows W.focusDown)       -- Move focus to the next window
        , ("M-k", windows W.focusUp)         -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
     --   , ("M-<Backspace>", promote)    -- Moves focused window to master, others maintain order
     --   , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
     --   , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack
        --, ("M-S-s", windows copyToAll)  
     --   , ("M-C-s", killAllOtherCopies)



        -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                -- Switch to next layout
    --    , ("M-C-M1-<Up>", sendMessage Arrange)
    --    , ("M-C-M1-<Down>", sendMessage DeArrange)
    --    , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", sendMessage ToggleStruts)         -- Toggles struts
    --    , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
    --    , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
    --    , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
    --    , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
    --    , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows

        , ("M-h", sendMessage Shrink)                       -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                       -- Expand horiz window width
       -- , ("M-C-j", sendMessage MirrorShrink)               -- Shrink vert window width
       -- , ("M-C-k", sendMessage MirrorExpand)               -- Exoand vert window width

    -- Workspaces
        , ("M-.", CWS.nextScreen)  -- Switch focus to next monitor
        , ("M-,", CWS.prevScreen)  -- Switch focus to prev monitor
        , ("M-S-a", CWS.shiftTo Next nonNSP >> CWS.moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-d", CWS.shiftTo Prev nonNSP >> CWS.moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- Scratchpads
    --    , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")

        

    --- My Applications (Super+Alt+Key)
        , ("M-c", spawn "copyq toggle")
        , ("M-S-t", spawn "nautilus")
        , ("M-g", spawn "emacsclient --alternate-editor='' --no-wait --create-frame")
        , ("M-w", spawn "firefox")
        , ("M-s", spawn "gopass ls --flat | dmenu | xargs --no-run-if-empty gopass show -c")
        , ("M-e b", spawn "emacsclient -c -a '' --eval '(ibuffer)'")         -- list emacs buffers
        , ("M-e d", spawn "emacsclient -c -a '' --eval '(dired nil)'")       -- dired emacs file manager
        , ("M-e m", spawn "emacsclient -c -a '' --eval '(mu4e)'")            -- mu4e emacs email client
        , ("M-e e", spawn "emacsclient -c -a '' --eval '(elfeed)'")          -- elfeed emacs rss client
        , ("M-e t", spawn "emacsclient -c -a '' --eval '(vterm)'")          -- eshell within emacs

        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("XF86MonBrightnessUp", spawn "light -A 10")
        , ("XF86MonBrightnessDown", spawn "light -U 10")
    
    
        , ("<Print>", spawn "scrotd 0")
        ]
        ++[ (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- was W.greedyView
                                      , ("S-", windows . W.shift)]
    ]

        -- Appending search engines to keybindings list
      --  ++ [("M-s " ++ k, S.promptSearch dtXPConfig' f) | (k,f) <- searchList ]
      --  ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
        ++ [("M-p" ++ k, f dtXPConfig) | (k,f) <- promptList ]
          where nonNSP          = CWS.WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = CWS.WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

  
------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

monocle  = renamed [Replace "monocle"]
           $ smartBorders $ limitWindows 20 Full


tabs     =smartBorders $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=25"
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }

spirals  = renamed [Replace "spirals"]
           $ smartSpacing 4
           $ spiral (6/7)


myLayout = avoidStruts $ ( tiled
                         ||| Mirror tiled
                         ||| tabs
                         ||| monocle
                         ||| spirals)
  where
    
     -- default tiling algorithm partitions the screen into two panes
     tiled   = smartBorders $ smartSpacing 5 $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "mpv"            --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "copyq"          --> doFloat
    , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
    , (className =? "zoom" <&&> title =? "Chat") --> doFloat
    , (className =? "zoom" <&&> title =? "") --> doFloat
    , className =? "zoom"     --> doShift (myWorkspaces !! 8)
    , className =? "firefox"     --> doShift (myWorkspaces !! 2)
    , className =? "Nautilus"     --> doShift (myWorkspaces !! 5 )
    , className =? "discord"     --> doShift (myWorkspaces !! 8 )
    , title =? "alsamixer"     --> doFloat
    , title =? "Manjaro Settings Manager"     --> doFloat
    
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.

myStartupHook :: X ()
myStartupHook = do
          -- spawnOnce "nitrogen --restore &" 
          spawnOnce "nm-applet &"
          spawnOnce "volumeicon &"
          spawnOnce "copyq &"
          spawnOnce "redshift &"
          spawnOnce "nextcloud &"
          spawnOnce "dropbox start &"
          spawnOnce "davmail &"
          spawnOnce "feh --bg-scale /home/ashfaqf/.config/nixpkgs/configs/Wallpapers/DSC_0749-1.jpg"
          
          spawnOnce "trayer --edge top --align right --widthtype request --padding 10 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x292d3e --height 24 &"
          spawnOnce "emacs --daemon &"
          setWMName "LG3D"


------------------------------------------------------------------------
-- TREE SELECT
------------------------------------------------------------------------
treeselectA :: TSConfig (X ()) -> X ()
treeselectA tsDefaultConfig = treeselectAction tsDefaultConfig
   [ Node (TSNode "hello"    "displays hello"      (spawn "xmessage hello!")) []
   , Node (TSNode "shutdown" "poweroff the system" (spawn "poweroff")) []
   , Node (TSNode "Restart" "restart the system" (spawn "reboot")) []
   , Node (TSNode "xmonad" "working with xmonad" (return ()))
     [ Node (TSNode "edit xmonad" "edit xmonad" (spawn (myTerminal ++ " -e vim ~/.xmonad/xmonad.hs"))) []
     , Node (TSNode "recompile xmonad" "recompile xmonad" (spawn "xmonad --recompile")) []
     , Node (TSNode "restart xmonad" "restart xmonad" (spawn "xmonad --restart")) []
     ]
   , Node (TSNode "brightness" "Sets screen brightness using xbacklight" (return ()))
       [ Node (TSNode "bright" "full power"            (spawn "light -S 100")) []
       , Node (TSNode "normal" "normal brightness (50%)" (spawn "light -S 50"))  []
       , Node (TSNode "dim"    "quite dark"              (spawn "light -S 10"))  []
       , Node (TSNode "darkest"    "value 1"              (spawn "light -S 1"))  []
       ]
   , Node (TSNode "Monitors" "Define monitor setting using xrandr" (return ()))
       [ Node (TSNode "Laptop only"    "Laptop Only"     (spawn "xrandr  --output eDP-1 --mode 3200x1800 --pos 0x0 --rotate normal --output HDMI-2 --off --output DP-1 --off")) []
       , Node (TSNode "Home" "Configuration for home" (spawn "xrandr --output HDMI-2 --mode 2560x1440 --scale 2x2 --pos 0x0 --rotate normal --output eDP-1 --mode 3200x1800 --pos 5120x0 --rotate normal")) []
       , Node (TSNode "Office"    "3 monitor config"  (spawn  "xrandr --output HDMI-2 --mode 1680x1050 --scale 2x2 --pos 6560x0 --rotate normal --output DP-1 --mode 1680x1050 --scale 2x2 --pos 3200x0 --rotate normal --output eDP-1 --mode 3200x1800 --pos 0x0 --rotate normal")) []
       ]
   ]

tsDefaultConfig :: TSConfig a
tsDefaultConfig = TSConfig { ts_hidechildren = True
                              , ts_background   = 0xdd292d3e
                              , ts_font         = "xft:mononoki Nerd Font:bold"
                              , ts_node         = (0xffd0d0d0, 0xff202331)
                              , ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
                              , ts_highlight    = (0xffffffff, 0xff755999)
                              , ts_extra        = 0xffd0d0d0
                              , ts_node_width   = 800
                              , ts_node_height  = 40
                              , ts_originX      = 0
                              , ts_originY      = 0
                              , ts_indent       = 80
                              , ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape), cancel)
    , ((0, xK_Return), select)
    , ((0, xK_space),  select)
    , ((0, xK_Up),     movePrev)
    , ((0, xK_Down),   moveNext)
    , ((0, xK_Left),   moveParent)
    , ((0, xK_Right),  moveChild)
    , ((0, xK_k),      movePrev)
    , ((0, xK_j),      moveNext)
    , ((0, xK_h),      moveParent)
    , ((0, xK_l),      moveChild)
    , ((0, xK_o),      moveHistBack)
    , ((0, xK_i),      moveHistForward)
    ]





------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/ashfaqf/.xmobar/xmobarrc"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/ashfaqf/.xmobar/xmobarrc"
  xmproc2 <- spawnPipe "xmobar -x 2 /home/ashfaqf/.xmobar/xmobarrc"
  xmonad $ docks def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalColor,
        focusedBorderColor = myFocusColor,

      -- key bindings
      --  keys               = myKeys, 
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = (isFullscreen --> doFullFloat) <+> myManageHook <+> manageDocks,
        handleEventHook    = myEventHook,

        logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x 
                        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        --, ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                        , ppHiddenNoWindows= \( _ ) -> ""       -- Only shows visible workspaces. Useful for TreeSelect.
                        , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        },
        
        startupHook        = myStartupHook
    } `additionalKeysP` myKeys



  


-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
