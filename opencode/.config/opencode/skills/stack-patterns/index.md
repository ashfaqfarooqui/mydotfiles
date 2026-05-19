# Stack Patterns Skill

Idiomatic conventions for the user's stack. Apply these when writing, reviewing, or refactoring code.

---

## Python

**Project structure**
```
src/[package]/
tests/
pyproject.toml       # prefer over setup.py
```

**Type hints** — always. Use `from __future__ import annotations` for forward refs.

**Linting/formatting**
- `ruff check .` — linting
- `ruff format .` — formatting (replaces black)
- `mypy .` — type checking

**Testing**
- `pytest` with `pytest-cov`
- Test files: `tests/test_[module].py`
- Fixtures in `conftest.py`
- Prefer `pytest.mark.parametrize` over repeated tests

**Virtual env**: `uv` preferred over pip/venv directly.

**Error handling**: raise specific exceptions, not bare `Exception`. Use `contextlib.suppress` for intentional swallows.

---

## Rust

**Toolchain**: stable, managed via `rustup`

**Error handling**
- Library code: `thiserror` for typed errors
- Application code: `anyhow` for ergonomic propagation
- Never `.unwrap()` in production paths — use `?` or handle explicitly

**Testing**
- Unit tests: `#[cfg(test)] mod tests` in the same file
- Integration tests: `tests/` directory
- Run: `cargo test`

**Linting**: `cargo clippy -- -D warnings` (treat warnings as errors in CI)

**Formatting**: `cargo fmt` — always before committing

**Common patterns**
- Prefer `impl Trait` over `Box<dyn Trait>` where possible
- Use `derive(Debug, Clone, PartialEq)` liberally
- `serde` for serialization — derive `Serialize, Deserialize`

---

## Shell

**Shebang**: `#!/usr/bin/env bash` (not `/bin/bash` for portability)

**Safety header** — always at top:
```bash
set -euo pipefail
```

**Portability**
- Prefer POSIX constructs when possible
- Use `[[ ]]` for bash conditionals (not `[ ]`)
- Quote all variable expansions: `"$var"` not `$var`

**Error messages**: write to stderr: `echo "error: ..." >&2`

**Linting**: `shellcheck script.sh` — fix all warnings

**Functions**: lowercase with underscores, `local` for all variables inside functions

**Exit codes**: 0 = success, non-zero = failure. Document non-zero codes in the script header.

---

## Docker

**Multi-stage builds** — always for compiled languages:
```dockerfile
FROM rust:1.82 AS builder
# ... build ...

FROM debian:bookworm-slim
COPY --from=builder /app/binary /usr/local/bin/
```

**Non-root user** — always:
```dockerfile
RUN useradd -r -u 1001 appuser
USER appuser
```

**`.dockerignore`** — always present. At minimum exclude:
```
.git
target/        # Rust
__pycache__/   # Python
*.pyc
.env
```

**Layer caching** — copy dependency files before source:
```dockerfile
# Python
COPY pyproject.toml .
RUN pip install .
COPY src/ src/

# Rust
COPY Cargo.toml Cargo.lock .
RUN cargo build --release --lib   # cache deps
COPY src/ src/
```

**Image size**: prefer `slim` or `alpine` variants for final stage. Use `distroless` for maximum security.

**Healthcheck**: include in production images.

**docker-compose**: use `depends_on` with `condition: service_healthy` for ordering.
