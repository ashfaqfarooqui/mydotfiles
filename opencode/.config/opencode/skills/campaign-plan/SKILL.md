---
name: campaign-plan
description: Turn a body of work (audit findings, a plan-mode plan, a bug report, a PR review, a task list) into a campaign plans folder at ~/.claude/plans/<slug>/ — MASTER.md tracker, AGENT_PROMPT.md rules, and one issue file per task, grouped into waves of disjoint files — so the work can be dispatched to agents with campaign-run. Use when the user wants to plan multi-issue work for agents to execute — "make a campaign", "split this into issues", "write the master plan and agent prompt", "turn this plan into a campaign" (a plan from plan mode in ~/.claude/plans/), or add issues to an existing campaign.
---

# campaign-plan

> **OpenCode edition** — a campaign dispatches to opencode agents only, never Claude/Anthropic
> models or agent kinds; issue headers carry a judgement-depth tag, not a model choice. See
> `../campaign-run/reference/run-modes.md`.

Produce the plans folder described in
[reference/format.md](reference/format.md). **Read that file first** — it is the contract
`campaign-run` and `campaign-audit` rely on. Templates live in `templates/`
(relative to this skill's directory).

Arguments (from the user's request): `<slug> <source: plan-mode plan | findings file | issue URL | description>`

## 0. Identify the input and decide whether an audit is needed

- Input is a **plan-mode plan** (a single `~/.claude/plans/<name>.md`, or "the plan we just
  made") → read [reference/from-plan-mode.md](reference/from-plan-mode.md) and follow it
  through steps 1–7. It says how the plan's sections map onto the campaign files and which gaps
  to fill.
- Input is concrete (a clear bug report, a review with line refs, a known task list, an
  existing `FINDINGS.md`) → skip the audit and plan from it.
- Input is a symptom or a vague worry ("this feels broken", "is X conformant?") and nobody knows
  the shape yet → suggest the `campaign-audit` skill first and stop until the user decides.

## 1. Set up

- Agree the `<slug>` and the ID prefix `<PFX>` with the user.
- `~/.claude/plans/<slug>/` — if it exists, this is an **extension**: read `MASTER.md`, continue
  numbering from the highest existing ID, and do not rewrite existing issues.
- Record the repo's absolute path, branch and commit.

## 2. Environment pre-check

Run every check in the *Environment pre-check* table in `format.md`. Ask the user about
attribution policy and protected paths if the repo does not answer it. The results go into
MASTER's **Dispatch hazards** and decide which run modes `campaign-run` may offer.

## 3. Measure the baseline

Run the gate (fmt / lint / build / tests, whatever the project has) once, in the foreground,
and record each number **with the command that produced it**. Different commands give different
counts for the same tree (`cargo test` vs `cargo nextest run`), and an unlabeled count sends
agents chasing phantom regressions.

## 4. Write the issues

One file per issue from `templates/ISSUE.md`. For each:
- State the defect, not the task, in the title.
- **Files you may touch** — explicit. This is what makes waves possible.
- **Acceptance criteria** — literal commands with expected results. A criterion that cannot be
  run is a wish.
- Pick the depth with the heuristic in `format.md`.
- Never "and N others" — enumerate, or give the command.
- Prefer fewer, well-bounded issues. Group mechanical work by file; separate anything with a
  judgement call so it can get deep.

## 5. Waves and decisions

- Group issues so each wave's file sets are disjoint. Ordering constraints go in the wave plan.
- Anything that needs the user's judgement becomes a `D<n>` decision in MASTER, not a guess
  inside an issue.

## 6. Write AGENT_PROMPT.md and MASTER.md

- `AGENT_PROMPT.md` from the template: fill **every** `<placeholder>` (project, gate, baselines,
  spec source, commit/attribution policy, protected paths, conventions, helpers to reuse). Delete
  the options that do not apply. `grep -n '<' AGENT_PROMPT.md` must show no leftovers.
- `MASTER.md` from the template: START HERE, wave plan, status table (one line per issue),
  decisions, depth policy, dispatch hazards, log.

## 7. Hand over

Show the user: the issue list with model and wave, the open decisions (ask them with
the question tool), and the safe run modes. Point to the `campaign-run` skill as the next step. Do
not dispatch anything from this skill.
