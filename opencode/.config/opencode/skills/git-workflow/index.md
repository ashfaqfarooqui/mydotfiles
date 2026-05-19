---
name: git-workflow
description: Branch-based git workflow for agents. Every feature or task gets its own branch, work is committed there, then merged back to the parent branch before starting the next task.
---

# Git Workflow

Every task or feature is done on its own branch. Never commit directly to `main` or the branch you started from.

---

## 1. Orient

Before doing anything, establish where you are:

```bash
git branch --show-current          # current (parent) branch
git status --porcelain             # must be clean before branching
git log --oneline -5               # recent history for context
```

If the working tree is dirty, stop and ask the user how to handle it — don't branch over uncommitted work.

---

## 2. Create a branch

Branch naming: `<type>/<short-slug>`

| Type | When |
|------|------|
| `feat/` | new feature or capability |
| `fix/` | bug fix |
| `refactor/` | restructuring without behavior change |
| `chore/` | deps, config, tooling |
| `docs/` | documentation only |
| `test/` | tests only |

```bash
git checkout -b feat/add-auth-module    # example
```

Slug rules: lowercase, hyphens only, ≤ 40 chars, describes the task not the implementation.

---

## 3. Work on the branch

- Use the `commit` skill after each logical unit of work — don't pile everything into one commit
- Keep commits atomic: one reason to change per commit
- Run linter/tests before each commit (per `stack-patterns` skill)
- Never merge or rebase during work unless the parent branch has a blocking change you need

---

## 4. Before merging — pre-merge checklist

```bash
# 1. All tests pass
pytest / cargo test / bats

# 2. Linter clean
ruff check . && mypy . / cargo clippy -- -D warnings / shellcheck

# 3. No untracked or uncommitted changes
git status --porcelain   # must be empty

# 4. Review your own diff
git diff main...HEAD     # substitute actual parent branch name
```

If any check fails, fix it on the feature branch before merging.

---

## 5. Merge back

```bash
# Switch to parent branch
git checkout main          # or whatever the parent branch was

# Fast-forward if possible, otherwise create a merge commit
git merge --no-ff feat/add-auth-module -m "merge feat/add-auth-module"
```

Use `--no-ff` so the branch history is preserved. The merge commit message: `merge <branch-name>` — nothing else needed.

After merging, confirm:

```bash
git log --oneline -5      # branch appears in history
git branch -d feat/add-auth-module   # delete the feature branch
```

---

## 6. Start the next task

Only after the merge is confirmed and the branch deleted. Go back to step 1.

---

## Rules

- Never commit directly to `main` or any branch the user designated as protected
- Never start a new branch while the previous one has uncommitted changes
- Never merge a branch that has failing tests or lint errors
- If a merge conflict arises, resolve it on the feature branch (not on main) — merge main into the feature branch, resolve, then re-merge
- Do not squash commits unless the user explicitly asks — keep the history readable
- One branch per task — don't bundle unrelated changes onto the same branch
