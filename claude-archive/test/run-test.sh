#!/usr/bin/env bash
# End-to-end test for claude-archive. Uses only scratch folders under /tmp,
# a throw-away age key and a local bare repo as origin.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
script="$here/../.local/bin/claude-archive"

tmp=$(mktemp -d /tmp/claude-archive-test.XXXXXX)
trap 'rm -rf "$tmp"' EXIT

fail() { echo "FAIL: $*" >&2; exit 1; }
pass() { echo "ok: $*"; }

export CLAUDE_DIR="$tmp/claude"
export ARCHIVE_DIR="$tmp/archive"
export AGE_RECIPIENT_FILE="$tmp/recipient.txt"
export STATE_DIR="$tmp/state"
unset IDLE_DAYS

key="$tmp/key.txt"
age-keygen -o "$key" 2>/dev/null
age-keygen -y "$key" >"$AGE_RECIPIENT_FILE"

marker="MARKER-7f3c9e1b-do-not-leak"
proj="$CLAUDE_DIR/projects/-home-u-proj"
old=11111111-1111-1111-1111-111111111111
new=22222222-2222-2222-2222-222222222222
gone=33333333-3333-3333-3333-333333333333

mkdir -p "$proj/$old/subagents" "$proj/$old/tool-results" "$proj/memory" \
  "$CLAUDE_DIR/plans/camp/target-x"
printf '{"msg":"%s"}\n' "$marker" >"$proj/$old.jsonl"
printf '{"sub":"%s"}\n' "$marker" >"$proj/$old/subagents/agent-a.jsonl"
printf 'tool output %s\n' "$marker" >"$proj/$old/tool-results/r1.txt"
printf '{"msg":"active %s"}\n' "$marker" >"$proj/$new.jsonl"
printf '{"msg":"gone %s"}\n' "$marker" >"$proj/$gone.jsonl"
printf '# memory %s\n' "$marker" >"$proj/memory/MEMORY.md"
printf '{"display":"%s"}\n' "$marker" >"$CLAUDE_DIR/history.jsonl"
printf '# master %s\n' "$marker" >"$CLAUDE_DIR/plans/camp/MASTER.md"
printf 'binary' >"$CLAUDE_DIR/plans/camp/target-x/foo.rlib"
printf 'cache' >"$CLAUDE_DIR/plans/camp/target-x/CACHEDIR.TAG"
find "$CLAUDE_DIR" -exec touch -d '3 days ago' {} +
touch -d '1 hour ago' "$proj/$new.jsonl"

git init -q --bare "$tmp/origin.git"
git init -q -b main "$ARCHIVE_DIR"
git -C "$ARCHIVE_DIR" config user.name test
git -C "$ARCHIVE_DIR" config user.email test@example.invalid
git -C "$ARCHIVE_DIR" config commit.gpgsign false
git -C "$ARCHIVE_DIR" remote add origin "$tmp/origin.git"
echo "# archive" >"$ARCHIVE_DIR/README.md"
git -C "$ARCHIVE_DIR" add README.md
git -C "$ARCHIVE_DIR" commit -q -m init
git -C "$ARCHIVE_DIR" push -q -u origin main

commits() { git -C "$ARCHIVE_DIR" rev-list --count HEAD; }
# decrypt <archive file> <dest dir>: single-file entries
decrypt() { age -d -i "$key" "$1" | zstd -dq; }

# --- run 1: first archive -------------------------------------------------
[[ -x "$script" ]] || fail "script missing: $script"
"$script" || fail "run 1 exit $?"
a="$ARCHIVE_DIR/projects/-home-u-proj"

[[ -f "$a/$old.jsonl.zst.age" ]] || fail "idle transcript not archived"
[[ ! -e "$a/$new.jsonl.zst.age" ]] || fail "active transcript archived"
pass "idle transcript archived, active transcript skipped"

out="$tmp/restore"
mkdir -p "$out"
age -d -i "$key" "$a/$old.jsonl.zst.age" | zstd -dq | tar -x -C "$out"
for f in "$old.jsonl" "$old/subagents/agent-a.jsonl" "$old/tool-results/r1.txt"; do
  cmp "$proj/$f" "$out/$f" || fail "restored $f differs"
done
cmp "$proj/memory/MEMORY.md" <(decrypt "$a/memory/MEMORY.md.zst.age") || fail "memory differs"
cmp "$CLAUDE_DIR/history.jsonl" <(decrypt "$ARCHIVE_DIR/history.jsonl.zst.age") || fail "history differs"
cmp "$CLAUDE_DIR/plans/camp/MASTER.md" <(decrypt "$ARCHIVE_DIR/plans/camp/MASTER.md.zst.age") ||
  fail "plan differs"
pass "decrypted transcript, session folder, memory, history and plan are byte-identical"

if find "$ARCHIVE_DIR" -path "$ARCHIVE_DIR/.git" -prune -o -name '*target-x*' -print | grep -q .; then
  fail "build output archived"
fi
pass "plan folder archived without target-x/ (foo.rlib, CACHEDIR.TAG)"

if git -C "$ARCHIVE_DIR" grep -q "$marker" HEAD; then fail "plain text in archive repo"; fi
if git -C "$tmp/origin.git" grep -q "$marker" main; then fail "plain text in origin"; fi
pass "git grep finds no plain text"

[[ "$(git -C "$tmp/origin.git" rev-parse main)" == "$(git -C "$ARCHIVE_DIR" rev-parse HEAD)" ]] ||
  fail "commit not pushed"
lp=$(cat "$STATE_DIR/last-push")
(( $(date +%s) - lp < 60 )) || fail "last-push not written"
pass "pushed, last-push written"

# --- run 2: no change -----------------------------------------------------
n=$(commits)
"$script" || fail "run 2 exit $?"
[[ "$(commits)" == "$n" ]] || fail "run with no change made a commit"
pass "second run with no change makes no commit"

# --- run 3: transcript removed, transcript and history shortened, ---------
# --- memory and plan emptied ---------------------------------------------
plan="$CLAUDE_DIR/plans/camp/MASTER.md"
archived() {
  sha256sum "$a/$gone.jsonl.zst.age" "$a/$old.jsonl.zst.age" "$ARCHIVE_DIR/history.jsonl.zst.age" \
    "$a/memory/MEMORY.md.zst.age" "$ARCHIVE_DIR/plans/camp/MASTER.md.zst.age"
}
before=$(archived)
rm "${proj:?}/$gone.jsonl"
printf 'x\n' >"$proj/$old.jsonl"
touch -d '50 hours ago' "$proj/$old.jsonl"
printf 'x\n' >"$CLAUDE_DIR/history.jsonl"
: >"$proj/memory/MEMORY.md"
: >"$plan"
"$script" || fail "run 3 exit $?"
[[ "$(archived)" == "$before" ]] || fail "removed, shorter append-only or empty source changed the archive"
git -C "$ARCHIVE_DIR" ls-files --error-unmatch "projects/-home-u-proj/$gone.jsonl.zst.age" \
  "history.jsonl.zst.age" "projects/-home-u-proj/memory/MEMORY.md.zst.age" >/dev/null ||
  fail "archive file removed from git"
[[ "$(commits)" == "$n" ]] || fail "removed, shorter append-only or empty source made a commit"
pass "removed source, shorter transcript, shorter history, empty memory and empty plan leave the archive unchanged"

# --- run 3b: shorter plan and shorter memory are archived -----------------
# Both are shorter than the archive copy (the manifest keeps the old size).
printf 'p\n' >"$plan"
printf 'm\n' >"$proj/memory/MEMORY.md"
touch -d '1 minute ago' "$plan" "$proj/memory/MEMORY.md"
"$script" || fail "run 3b exit $?"
cmp "$plan" <(decrypt "$ARCHIVE_DIR/plans/camp/MASTER.md.zst.age") || fail "shorter plan not archived"
cmp "$proj/memory/MEMORY.md" <(decrypt "$a/memory/MEMORY.md.zst.age") || fail "shorter memory not archived"
[[ "$(commits)" == $((n + 1)) ]] || fail "shorter plan and memory did not make one commit"
pass "shorter plan and shorter memory archived in one commit"

# --- run 4: changed plan is archived again --------------------------------
n=$(commits)
printf 'more\n' >>"$CLAUDE_DIR/plans/camp/MASTER.md"
"$script" || fail "run 4 exit $?"
cmp "$CLAUDE_DIR/plans/camp/MASTER.md" <(decrypt "$ARCHIVE_DIR/plans/camp/MASTER.md.zst.age") ||
  fail "changed plan not archived again"
[[ "$(commits)" == $((n + 1)) ]] || fail "changed plan did not make one commit"
pass "changed plan archived again in one commit"

# --- run 5: plain file in the archive is refused --------------------------
n=$(commits)
echo "$marker" >"$ARCHIVE_DIR/notes.txt"
if "$script" 2>/dev/null; then fail "plain file in archive was accepted"; fi
[[ "$(commits)" == "$n" ]] || fail "plain file was committed"
git -C "$ARCHIVE_DIR" reset -q
rm "$ARCHIVE_DIR/notes.txt"
pass "plain file in the archive stops the run"

rm "$ARCHIVE_DIR/history.jsonl.zst.age"
if "$script" 2>/dev/null; then fail "deletion in archive was accepted"; fi
[[ "$(commits)" == "$n" ]] || fail "deletion was committed"
git -C "$ARCHIVE_DIR" reset -q
git -C "$ARCHIVE_DIR" show HEAD:history.jsonl.zst.age >"$ARCHIVE_DIR/history.jsonl.zst.age"
pass "a file deleted from the archive stops the run"

# --- run 6: push to a missing remote --------------------------------------
echo 1000 >"$STATE_DIR/last-push"
git -C "$ARCHIVE_DIR" remote set-url origin "$tmp/missing.git"
printf 'again\n' >>"$CLAUDE_DIR/plans/camp/MASTER.md"
if "$script" 2>/dev/null; then fail "push to missing remote exit 0"; fi
[[ "$(cat "$STATE_DIR/last-push")" == 1000 ]] || fail "last-push changed after failed push"
pass "push to a missing remote exits non-zero and keeps last-push"

echo "all tests passed"
