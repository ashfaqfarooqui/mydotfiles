#!/usr/bin/env python3
# Backs services/AgentsUsage.qml's "Agents" panel — aggregates local coding-
# agent usage into "tokens by day" (last 7 days) and "tokens by model" for
# each of two sources: Claude Code (from ~/.claude/projects transcripts) and
# opencode (via `opencode db`, its own official CLI for querying its sqlite
# store — not touched directly). No network calls, no credentials read —
# Session/Weekly live quota bars were explicitly scoped out for Claude Code
# (those need Anthropic's undocumented OAuth usage endpoint); opencode has
# no equivalent concept.
#
# ~/.claude/usage-data/session-meta/*.json looked like the obvious Claude
# source at first, but on this machine it stopped updating a week before
# "today" (stale snapshot, not a live log) — the transcripts are the only
# source that's actually current, so both Claude charts are built from them.
import json
import glob
import os
import subprocess
import sys
from collections import defaultdict
from datetime import datetime, timedelta, timezone

HOME = os.path.expanduser("~")


def last_n_day_buckets(n=7):
    today = datetime.now(timezone.utc).date()
    return [(today - timedelta(days=i)) for i in range(n - 1, -1, -1)]


def days_to_series(totals_by_iso_date):
    days = last_n_day_buckets()
    out = []
    for i, d in enumerate(days):
        label = "Today" if i == len(days) - 1 else d.strftime("%a")
        out.append({"day": label, "tokens": totals_by_iso_date.get(d.isoformat(), 0)})
    return out


def claude_usage_total(usage):
    return (
        usage.get("input_tokens", 0)
        + usage.get("output_tokens", 0)
        + usage.get("cache_creation_input_tokens", 0)
        + usage.get("cache_read_input_tokens", 0)
    )


def pretty_claude_model(model):
    # "claude-sonnet-5" -> "Sonnet 5", "claude-haiku-4-5-20251001" ->
    # "Haiku 4.5" — strip the "claude-" prefix, a trailing date suffix,
    # and title-case what's left.
    name = model.removeprefix("claude-")
    name = name.rsplit("-", 1)[0] if name[-8:].isdigit() and len(name) > 8 else name
    parts = name.split("-")
    if len(parts) >= 2 and all(p.isdigit() for p in parts[1:]):
        return f"{parts[0].capitalize()} {'.'.join(parts[1:])}"
    return name.replace("-", " ").title()


def claude_stats():
    by_day = defaultdict(int)
    by_model = defaultdict(int)

    for path in glob.glob(f"{HOME}/.claude/projects/**/*.jsonl", recursive=True):
        try:
            if os.path.getsize(path) > 100 * 1024 * 1024:
                continue  # skip anything absurdly large
            with open(path, "r", errors="ignore") as f:
                for line in f:
                    line = line.strip()
                    if not line or '"usage"' not in line:
                        continue
                    try:
                        rec = json.loads(line)
                    except json.JSONDecodeError:
                        continue
                    msg = rec.get("message")
                    if not isinstance(msg, dict):
                        continue
                    usage = msg.get("usage")
                    model = msg.get("model")
                    if not isinstance(usage, dict) or not model:
                        continue
                    total = claude_usage_total(usage)
                    by_model[model] += total

                    ts = rec.get("timestamp", "")
                    if ts:
                        by_day[ts[:10]] += total
        except OSError:
            continue

    ranked = sorted(
        ((m, t) for m, t in by_model.items() if t > 0 and not m.startswith("<")),
        key=lambda kv: kv[1], reverse=True,
    )
    tokens_by_model = [{"model": pretty_claude_model(m), "tokens": t} for m, t in ranked[:8]]

    return {"tokensByDay": days_to_series(by_day), "tokensByModel": tokens_by_model}


def opencode_db(query):
    # `opencode db` is opencode's own supported CLI for querying its sqlite
    # store (opencode.db, 1GB+ on this machine) — using it instead of
    # touching the .db file directly means this keeps working even if
    # opencode's internal schema changes.
    result = subprocess.run(
        ["opencode", "db", query, "--format", "json"],
        capture_output=True, text=True, timeout=15,
    )
    if result.returncode != 0:
        return []
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError:
        return []


def opencode_stats():
    by_day = {}
    for row in opencode_db(
        "SELECT date(time_created/1000,'unixepoch') as day, "
        "SUM(COALESCE(json_extract(data,'$.tokens.input'),0) + COALESCE(json_extract(data,'$.tokens.output'),0)) as tokens "
        "FROM message WHERE json_extract(data,'$.tokens') IS NOT NULL "
        "AND time_created > (strftime('%s','now') - 7*86400) * 1000 "
        "GROUP BY day"
    ):
        if row.get("day"):
            by_day[row["day"]] = row.get("tokens") or 0

    by_model = []
    for row in opencode_db(
        "SELECT json_extract(data,'$.modelID') as model, "
        "SUM(COALESCE(json_extract(data,'$.tokens.input'),0) + COALESCE(json_extract(data,'$.tokens.output'),0)) as tokens "
        "FROM message WHERE json_extract(data,'$.tokens') IS NOT NULL "
        "GROUP BY model ORDER BY tokens DESC LIMIT 8"
    ):
        model = row.get("model")
        tokens = row.get("tokens") or 0
        if model and tokens > 0:
            by_model.append({"model": model, "tokens": tokens})

    return {"tokensByDay": days_to_series(by_day), "tokensByModel": by_model}


def main():
    out = {"claude": claude_stats()}
    try:
        out["opencode"] = opencode_stats()
    except (subprocess.SubprocessError, OSError):
        out["opencode"] = {"tokensByDay": [], "tokensByModel": []}
    json.dump(out, sys.stdout)


if __name__ == "__main__":
    main()
