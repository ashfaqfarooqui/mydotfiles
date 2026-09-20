---
name: campaign-run
description: Orchestrate a campaign plans folder at ~/.claude/plans/<slug>/ — resume from MASTER.md, pick the next issues, let the user choose how to run them (in-session subagents, git worktrees, or Herdr panes), dispatch with AGENT_PROMPT.md, verify each result independently, and record outcomes back into the plan files. Use when the user wants to continue or execute a planned campaign — "continue openscenario-rs", "run the next wave", "dispatch SW-57", "where were we on <project>?".
---

# campaign-run

> **OpenCode edition** — every dispatch runs an opencode agent on the calling session's model.
> Never dispatch Claude/Anthropic models or agent kinds (see run-modes.md); use another agent
> kind only when the user explicitly asks for it.

The plan files follow `../campaign-plan/reference/format.md` (relative to
this skill's directory) — read it if the folder is unfamiliar. Run modes: [reference/run-modes.md](reference/run-modes.md).
Dispatch prompt: [reference/dispatch-prompt.md](reference/dispatch-prompt.md).

Arguments (from the user's request): `<slug> <issue IDs | 'next' | 'status'>` (`status` = steps 1–2 only, no dispatch).

## 1. Resume

- Read `~/.claude/plans/<slug>/MASTER.md`: START HERE, Status, Decisions, Dispatch hazards.
- Check the record against reality: `git -C <repo> log --oneline` for the commits the status
  table claims, `git status` for uncommitted work. Report any disagreement before acting on it.
- If `AGENT_PROMPT.md` still has `<placeholders>`, stop and fix that first.

## 2. Pick the work

Propose the next issue(s): ready (`☐`), dependencies done, no open decision blocking them, and
— for more than one — in the same wave with disjoint file sets. Read each chosen issue **to the
bottom**. Confirm the set with the user.

## 3. Choose the run mode — the user decides

Re-run the pre-checks from run-modes.md (they may have changed since planning), then ask with
the question tool. Offer each mode with its pre-check result in the description; mark a mode
unsafe rather than hiding it, and say why. Recommend **subagent (serial)** unless the user asked
for parallelism and the checks allow it.

## 4. Dispatch

Build each prompt from dispatch-prompt.md: absolute paths, the model inherited from the calling
opencode session (never a per-issue backend — see run-modes.md), and any decision (`D<n>`) that
overrides the doc. If you notice you are about to write "ignore X in AGENT_PROMPT.md", edit
`AGENT_PROMPT.md` instead and dispatch without the override.

Mark the issue `◐` in the status table before dispatching. Then follow the mode's recipe.

## 5. Verify independently

An agent's report is a claim. For each finished issue:
- `git show --stat <commit>` — changes stayed inside **Files you may touch** (or the deviation
  was reported and is justified).
- Re-run the acceptance commands yourself, **in the foreground** (all of them if cheap, a
  targeted subset otherwise, and say which).
- Commit message matches the policy (prefix, attribution rule, signed if required).
- If anything fails, do not record it as done: send it back to the agent (subagent task or Herdr
  prompt) or report to the user.

## 6. Record

- Status row → `☑`, commit hash, one-line outcome.
- Append `## Outcome (<date>, <commit>)` to the **issue file** with the report's substance —
  changed files, before/after numbers, questions decided, notes for the next agent. MASTER gets
  no narrative.
- Every *Out of scope, found anyway* item → a new issue file (next free ID, `**Raised by:**`),
  a status row, and a mention to the user. Findings worth keeping → append to `FINDINGS.md`.
- A lesson every future agent needs → a rule in `AGENT_PROMPT.md`, not a note in the dispatch.
- Baselines moved → update them in `AGENT_PROMPT.md` with the command.
- One dated line in MASTER's Log.
- **Overwrite** START HERE: state, next steps, live hazards, what awaits the user.
- If `MASTER.md` > ~30 KB, move old material to `ARCHIVE.md`.

## 7. Continue or stop

Continue with the next issues only if the user asked for more than one round. Never push,
merge to a shared branch, or unlock signing on the user's behalf — list those under
"Awaiting the user" in START HERE.
