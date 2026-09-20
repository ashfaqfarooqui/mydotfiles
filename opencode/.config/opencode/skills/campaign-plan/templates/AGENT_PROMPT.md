# Agent prompt — <slug>

`campaign-run` hands this to one agent per issue, with `<ISSUE-ID>` substituted.
This file is the single source of truth for agent behaviour: change it here, never override it
in a dispatch.

---

## Prompt template

> You are working on one issue in the project at `<absolute repo path>`
> (`<name>`: <one-line description>).
>
> **Your task is `<ISSUE-ID>` and nothing else.**
> Read these files first, in order:
> 1. `<plans dir>/AGENT_PROMPT.md` (this file — the rules)
> 2. `<plans dir>/<ISSUE-ID>-*.md` (your issue — read it to the bottom; dated appendices are part of it)
> 3. `<plans dir>/FINDINGS.md` (only the findings your issue cites)
>
> Work only on the files your issue lists under **Files you may touch**. If a fix needs a file
> outside that list, stop and report why rather than editing it — another agent may own it.
>
> <If the project has a spec: **`<spec path>` is the specification.** When the issue and the spec
> disagree, the spec wins and you say so in your report.>
>
> When done, report using the template at the bottom of this file.

---

## Rules for every issue

### Scope
One issue, one agent, one set of files. If you find a bug outside your scope, write it up in
your report under *Out of scope* — do not fix it.

### Verify by running, not by reading
Every issue has **Acceptance criteria** with literal commands. Run them. Paste real output.
Do not claim a criterion passes without running it. Capture the numbers **before** you change
anything and again after; a result without a before is not evidence.

Run long commands **in the foreground** and wait for them. Do not background a gate and poll it.

The gate for this project:

```
<fmt / lint / build / test commands>
```

Baselines, each with the command that produced it (at `<commit>`):
- `<command>` → <number>

### Do not "fix" tests to make them pass
If a test fails after your change, decide which is wrong — the test or the code — and say so
in the report. Never weaken a test, an assertion, or an example to get green. Where an expected
value genuinely must change, derive the new one and record the derivation.

### Never write "and N others"
List every instance, or give the command that lists them.

### Git safety
Other sessions may be live in this repository.
- **Never** run bare `git stash` / `git stash pop`, `git checkout -- <path>`, `git restore`,
  `git reset --hard`, or `git clean`. To set work aside, make a WIP commit.
- **Never** pass `--no-verify` or `--no-gpg-sign`. If a hook or signing fails, stop and report;
  leave the work staged with the message ready.
- **Do not push.**
- <Commit or not: "Commit your work" | "Do not commit; the orchestrator stages by path.">

### Commit style
One commit per issue, prefixed with the issue id:
`<type>(<scope>): <ISSUE-ID> <summary>`

<Attribution policy — pick one and delete the other:>
- **No AI-attribution trailers.** No `Co-Authored-By`, no `Claude-Session:`, no "Generated
  with". The message ends at the last line of real content.
- **Use the harness attribution trailers** from your session's system reminder.

### Protected paths
<Paths never to touch or clean, and why. e.g. "`output/` is stale and gitignored — ignore it.">

---

## Depth policy

Depth records judgement load, not a backend — every dispatch runs on the calling opencode
session's model.

| Depth | Use for | Issues |
|---|---|---|
| fast | Mechanical, fully specified edits | |
| standard | Bounded changes where the compiler/tests catch mistakes | |
| deep | Judgement against the spec; anywhere a wrong answer is silent | |

---

## Conventions in this codebase worth knowing

<Idioms, traps, and existing helpers to reuse before writing new ones. file:line references.>

---

## Reporting template

```
## <ISSUE-ID> — <title>

### Changed
- file:line — what and why

### Acceptance criteria
- [ ] <criterion>  →  <actual command output>

### Baselines
|         | before | after |
|---------|--------|-------|
| <gate>  |        |       |

### Questions I had to decide
- <where the issue and the spec/code disagreed, and which I followed>

### Out of scope, found anyway
- <finding> — belongs in <issue or "new">

### Notes for the next agent
- <anything that changes assumptions downstream>
```
