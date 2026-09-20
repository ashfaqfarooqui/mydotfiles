# Converting a plan-mode plan into a campaign

Plan mode writes a single Markdown plan file — Claude Code writes it to
`~/.claude/plans/<random-name>.md`; an opencode plan works the same way, wherever the session
saved it (e.g. `pwd-frolicking-pike.md`). Plan agents may leave companions next to it named
`<same-name>-agent-<id>.md`. These plans are written to be executed in one session; a campaign
splits them so separate agents can execute them and a later session can resume.

## Finding the plan

- A path in the arguments → use it.
- "the plan", "the last plan" → `ls -t ~/.claude/plans/*.md | head` and confirm the pick with the
  user (show the title and date). Include any `<name>-agent-*.md` companions as extra input.
- Plans in the campaign subfolders (`~/.claude/plans/<slug>/`) are campaign files, not plan-mode
  plans.

## Section mapping

Plan-mode plans have no fixed schema, but the planning workflow asks for these parts. Map them:

| Plan-mode part | Goes to |
|---|---|
| Title, **Context** (why, what prompted it, intended outcome) | MASTER title + **Origin** paragraph; the relevant slice becomes each issue's *Why this exists* |
| Chosen approach / design | Issues' *What to build*; cross-cutting constraints → `AGENT_PROMPT.md` rules or conventions |
| Implementation steps / phases | **Issues** — see splitting below |
| Critical files to modify | Each issue's **Files you may touch**; the file overlaps between issues decide the **waves** |
| Existing functions/utilities to reuse | `AGENT_PROMPT.md` *Conventions* ("reuse before adding"), and the issue that needs them |
| **Verification** section | Each issue's **Acceptance criteria** (the part that checks that issue), and the final issue or the wave gate (the end-to-end part) |
| Questions left open, "TBD", alternatives not chosen | `D<n>` decisions in MASTER — ask the user; do not choose silently |
| Risks, caveats | MASTER **Dispatch hazards** or the relevant issue's *Notes* |

## Splitting steps into issues

- One issue = one agent's worth of work with its own file set and its own checkable result.
  Merge steps that touch the same files and cannot be verified apart; split a step that touches
  unrelated areas.
- A step that says "decide", "investigate" or "evaluate" becomes its own issue (usually deep) that
  the later steps depend on, or a `D<n>` decision if the user should make the call.
- Keep the plan's order as `Depends on`. Steps with no dependency between them and disjoint files
  go into the same wave.

## Gaps to fill — plan-mode plans usually lack these

Plan-mode plans are written for a reader who has the whole session in context. An agent that
starts cold needs more, so before writing the issues:

- **Literal acceptance commands and expected results.** "Run the tests" is not a criterion. Run
  the gate now, record the baselines with their commands (step 3 of SKILL.md).
- **Explicit file lists.** "Update the exporter" → name the files; look them up in the repo.
- **Line references** for anything the plan describes from memory — check they still exist.
- **Repo path, branch, commit** — plans often assume the cwd.
- **The environment pre-check** — plans never have it.

If the plan is too vague to turn into issues with runnable criteria, say which parts are vague
and ask, or suggest the `campaign-audit` skill for those parts.

## Keep the source

- Do not move or edit the plan-mode file; it is the record of what was approved.
- Link it from MASTER's Origin: `Source plan: ~/.claude/plans/<name>.md (approved <date>)`.
- Where an issue deliberately departs from the plan, say so in the issue and why.
