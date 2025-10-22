return {
  {
    "ThePrimeagen/git-worktree.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
    config = function()
      vim.g.git_worktree = {
        change_directory_command = "cd",
        update_on_change = true,
        update_on_change_command = "e .",
        clearjumps_on_change = true,
        confirm_telescope_deletions = true,
        autopush = false,
      }
      local Hooks = require("git-worktree.hooks")
      local config = require("git-worktree.config")

      Hooks.register(Hooks.type.SWITCH, function(path, prev_path)
        vim.notify("Moved from " .. prev_path .. " to " .. path)
        -- check if current buffer is an oil buffer
        if vim.fn.expand("%"):find("^oil:///") then
          -- switch to new cwd in oil
          require("oil").open(vim.fn.getcwd())
        else
          -- use built in hook for non oil buffers
          Hooks.builtins.update_current_buffer_on_switch(path, prev_path)
        end
      end)

      Hooks.register(Hooks.type.DELETE, function()
        vim.cmd(config.update_on_change_command)
      end)
      require("telescope").load_extension("git_worktree")
    end,
    keys = {
      {
        "<leader>gws",
        function()
          require("telescope").extensions.git_worktree.git_worktrees()
        end,
        desc = "[G]it [W]orktree",
      },
      {
        "<leader>gwc",
        function()
          require("telescope").extensions.git_worktree.create_git_worktree()
        end,
        desc = "[G]it [W]orktree [C]reate",
      },
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    optional = true,
    opts = function(_, opts)
      local actions = require("telescope.actions")

      -- Add git-worktree specific telescope configuration
      opts.extensions = opts.extensions or {}
      opts.extensions.git_worktree = {
        theme = "dropdown",
        layout_config = {
          width = 0.8,
          height = 0.6,
        },
        mappings = {
          i = {
            ["<C-d>"] = function(prompt_bufnr)
              -- Delete worktree (custom action)
              local selection = require("telescope.actions.state").get_selected_entry()
              if selection then
                local worktree_path = selection.path
                local confirm = vim.fn.input("Delete worktree '" .. selection.display .. "'? (y/N): ")
                if confirm:lower() == "y" then
                  require("git-worktree").delete_worktree(worktree_path)
                  actions.close(prompt_bufnr)
                end
              end
            end,
          },
          n = {
            ["dd"] = function(prompt_bufnr)
              -- Delete worktree in normal mode
              local selection = require("telescope.actions.state").get_selected_entry()
              if selection then
                local worktree_path = selection.path
                local confirm = vim.fn.input("Delete worktree '" .. selection.display .. "'? (y/N): ")
                if confirm:lower() == "y" then
                  require("git-worktree").delete_worktree(worktree_path)
                  actions.close(prompt_bufnr)
                end
              end
            end,
          },
        },
      }
    end,
  },
}
