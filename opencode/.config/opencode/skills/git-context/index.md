# Git Context Skill

Orient yourself in the git repo before doing any work. Run these commands silently at session start — do not show raw output to the user unless relevant.

## Always run on start

```bash
git rev-parse --show-toplevel          # confirm repo root
git status                             # working tree state
git log --oneline -10                  # recent history
git branch --show-current             # current branch
git remote get-url origin 2>/dev/null # remote URL for vault log
```

## For review, refactor, debug sessions

```bash
git diff                               # unstaged changes
git diff --cached                      # staged changes
git diff HEAD~1                        # last commit vs current
git diff main...HEAD                   # branch changes vs main
```

Use the specific range the user mentions. If they say "review my last commit", use `HEAD~1..HEAD`. If they say "review this branch", use `main...HEAD`.

## What to extract

From these commands, derive:
- **Project name**: `basename $(git rev-parse --show-toplevel)`
- **Remote URL**: for vault log entry
- **Changed files**: for scoping work
- **Recent context**: what happened before this session

## Rules

- Never assume the working directory — always read git status first
- If `git rev-parse` fails, you're not in a git repo — ask the user where the project root is
- Never run `git add`, `git commit`, `git push`, or any mutating git command unless the user explicitly asks
