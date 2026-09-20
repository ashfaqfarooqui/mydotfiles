# Run modes

The user chooses. Present all four with their pre-check result; recommend, don't decide.

## Pre-checks (re-run before offering)

```bash
git -C <repo> status --short                 # dirty tree → worktrees branch from HEAD without it
git -C <repo> worktree list                  # other live checkouts share the index/refs
git -C <repo> config --get commit.gpgsign    # true → a locked key stalls agents
test "${HERDR_ENV:-}" = 1 && echo herdr      # herdr mode only inside Herdr
```

Relative path dependencies (any hit makes worktree mode unsafe unless the worktree sits at the
same relative depth):

```bash
grep -nE '^\s*path\s*=' <repo>/Cargo.toml <repo>/*/Cargo.toml 2>/dev/null
grep -nE '"(file|link):' <repo>/package.json 2>/dev/null
grep -nE '^\s*-e\s+\.\.|@ file:' <repo>/requirements*.txt <repo>/pyproject.toml 2>/dev/null
grep -nE '=>\s*\.\.' <repo>/go.mod 2>/dev/null
```

Also read MASTER's **Dispatch hazards** — it may record project-specific reasons (as in
scenario-weaver: "worktree isolation cannot work, the path dep is relative").

## 1. Subagent, serial — default

- **When:** always safe. The only mode for issues in different waves or with overlapping files.
- **How:** `Agent` tool, `subagent_type: general-purpose`, model inherited from the calling
  opencode session, prompt from dispatch-prompt.md, cwd = main checkout. One at a time; verify
  and record each before the next.
- **Follow-up:** resume the same subagent session (or Herdr prompt) to fix a failed verification.

## 2. Git worktrees, parallel

- **When:** issues are in the same wave (disjoint files), no relative path deps (or they resolve
  from the worktree location), and the user wants parallelism.
- **How, subagents:** `Agent` with `isolation: "worktree"`, one per issue, all in a single
  message. The result returns the worktree path and branch.
- **How, manual (for Herdr or when you need control over location/branch names):**
  ```bash
  git -C <repo> worktree add <repo>/../<slug>-wt/<ISSUE-ID> -b campaign/<ISSUE-ID>
  ```
  Place worktrees where relative deps still resolve (a sibling at the same depth as the main
  checkout).
- **Integrate:** after verifying each branch, merge in dependency order
  (`git merge --ff-only` where possible, otherwise ask the user whether to rebase or merge).
  Re-run the full gate on the integrated result, in the foreground.
- **Clean up:** remove worktrees you created only after their branch is merged, and only
  after `git -C <wt> status --short` is empty. Never remove a worktree you did not create.

## 3. Herdr panes

- **When:** the user wants to watch or talk to each agent, the task is long-running, or they want
  a different agent kind (e.g. codex) for some issues. Requires `HERDR_ENV=1`.
- **How:** invoke the `herdr` skill and follow it for all Herdr commands — do not guess the CLI.
  In outline: one sibling pane per issue (cwd = main checkout for a single issue, or that
  issue's worktree from mode 2 for parallel work), start an agent with a unique name such as the
  lowercased issue id (`osr-07`), send the dispatch prompt, wait, read the result.
- **Model:** the calling opencode session's model — dispatch `--kind opencode` and pass the
  model after `--` (e.g. `-- --model zai-coding-plan/glm-5.3`), checking `herdr agent` for the
  kind's options first. Never default to `claude`/Anthropic kinds or models; use another kind
  only when the user explicitly asks for it.
- **Results:** if the report scrolls off, ask the agent to write it to a file in the scratchpad
  and read that (as the herdr skill describes).
- Parallel Herdr panes in **one** checkout follow mode 4's rules.

## 4. Parallel in one checkout, no commits

- **When:** parallelism is wanted but worktrees are unsafe (relative path deps), and file sets are
  disjoint.
- **How:** subagents or Herdr panes, all in the main checkout, each told **not to commit**.
  After each finishes and is verified, the orchestrator stages **by path**
  (`git add <files from the issue list>`) and commits with the issue's message.
- **Risk:** agents share one build directory and one index; a build or test run by one agent can
  see another's half-done edits. Say so to the user when offering this mode, and re-run the
  gate after every commit.
