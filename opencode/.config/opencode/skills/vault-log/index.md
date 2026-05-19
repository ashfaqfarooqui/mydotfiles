# Vault Log Skill

Write a timestamped activity entry to the project's Obsidian coding log. Do this at the end of every coding session.

## Determine the project name

Use the repo directory name (e.g. `basename $(git rev-parse --show-toplevel)`). If not in a git repo, ask the user. Confirm once per session — don't ask again.

## Log file

`Notes/coding-log-[project-name].md`

One file per project. Always append — never overwrite.

## Check if file exists

```bash
obsidian search query="coding-log-[project-name]" limit=1
```

If it doesn't exist, create it first:
```bash
obsidian create name="coding-log-[project-name]" silent
obsidian property:set name="type" value="coding-log" file="coding-log-[project-name]"
obsidian property:set name="project" value="[project-name]" file="coding-log-[project-name]"
```

Then add a header:
```bash
obsidian append file="coding-log-[project-name]" content="# Coding Log — [project-name]\n\nTimestamped session history. Appended by coding agents.\n\n---\n"
```

## Entry format

Append this block at the end of the file:

```
## YYYY-MM-DD HH:MM — [agent-name]

**Project:** [project-name] — [repo absolute path or remote URL if available]
**Action:** requirements | architecture | tasks | code | tests | review | debug | refactor
**Summary:** one sentence describing what happened
**Files:** path/to/file1, path/to/file2
**Tasks covered:** [task description or #id from tasks doc]
**Requirements linked:** [REQ-n or description, if applicable]

---
```

Get the timestamp with: `date +"%Y-%m-%d %H:%M"`

Get the remote URL with: `git remote get-url origin 2>/dev/null || echo "local"`

## Append command

```bash
obsidian append file="coding-log-[project-name]" content="[entry]"
```

## Rules

- Always append — never overwrite or read back the full log
- Fill in every field. If a field doesn't apply, write `—`
- Files paths should be relative to repo root
- Write the entry even if the session was cut short
