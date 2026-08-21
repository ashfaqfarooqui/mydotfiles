---
name: commit
description: Stage and commit changes with meaningful, concise commit messages, split into logically separate commits by concern — no AI attribution, no Co-Authored-By footer, no force-push
---

# Git Commit Skill

Create clean, meaningful git commits, grouped by concern.

## Rules

- **Never** include "Co-Authored-By", "Generated with", "signed by Claude", or any reference to Claude, AI, or any model — in the commit message OR anywhere else (no signing on Claude's behalf, no committer identity changes)
- **Never** force-push (`--force`, `--force-with-lease`), even on retry after a failed push — report the failure and let the user decide
- **Never** skip hooks (`--no-verify`) or bypass GPG signing config
- Subject line under 72 characters
- Imperative mood: "add", "fix", "update", "remove", "refactor" — not "added"
- No trailing period on the subject line
- Use a body only when the why needs explaining — keep it brief
- Match the repo's existing commit style (check `git log --oneline -5` first)
- Use conventional commit prefixes (`feat:`, `fix:`, `chore:`, etc.) only if the repo already uses them
- Group changes by concern: one commit per distinct config/feature/fix, not one giant commit

## Steps

1. Check for `.git/index.lock`. If present, do NOT delete or retry — identify the holding process (`lsof .git/index.lock` or `fuser`), report it, and stop.
2. Run the project's build/typecheck command if one is discoverable. If it fails, stop and report — do not commit. If none is discoverable, skip this step rather than guessing.
3. Run `git status --porcelain` and `git diff` in parallel to see what changed
4. Run `git log --oneline -5` to match the repo's commit style
5. Group the changed files into logically separate commits by concern; draft a message per group
6. Propose the grouping and messages to the user and wait for explicit approval before staging or committing anything
7. Once approved, stage and commit each group separately (avoid `git add -A` — be selective per commit) — no attribution footer of any kind
8. Show `git log --oneline -N` (N = number of new commits, or 3, whichever is larger) to confirm

## Message format

```
<type>: <short description>

[optional body: explain why, not what]
```

## Examples

```
fix: prevent race condition in session cleanup
feat: add keyboard shortcut for quick file switch  
refactor: extract auth logic into separate module
chore: update dependencies to latest patch versions
docs: clarify setup steps for local development
```
