---
name: commit
description: Stage and commit changes with a meaningful, concise commit message — no AI attribution, no Co-Authored-By footer
---

# Git Commit Skill

Create clean, meaningful git commits.

## Rules

- **Never** include "Co-Authored-By", "Generated with", or any reference to Claude, AI, or any model in the commit message or footer
- Subject line under 72 characters
- Imperative mood: "add", "fix", "update", "remove", "refactor" — not "added"
- No trailing period on the subject line
- Use a body only when the why needs explaining — keep it brief
- Match the repo's existing commit style (check `git log --oneline -5` first)
- Use conventional commit prefixes (`feat:`, `fix:`, `chore:`, etc.) only if the repo already uses them

## Steps

1. Run `git status --porcelain` and `git diff --cached` in parallel to see what's staged
2. If nothing is staged, run `git diff` to see unstaged changes, then stage relevant files selectively — avoid `git add -A`
3. Run `git log --oneline -5` to match the repo's commit style
4. Analyze the diff: what changed and *why* — focus the message on the why
5. Propose a commit message to the user
6. Run `git commit -m "<message>"` — no attribution footer of any kind
7. Show `git log --oneline -3` to confirm

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
