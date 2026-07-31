#!/usr/bin/env bash
# Watch the freeze-log heartbeat and capture forensics the moment it stalls.
#
# Why this exists: the freeze under investigation (see design-docs/scroll-bars.md)
# wedges Emacs's main thread inside a redisplay flush that never returns.  By the
# time it is noticed by hand, minutes or hours have passed and the most useful
# evidence -- what the process was doing right as it stopped, and what macOS was
# doing around it -- is either gone or buried.  This captures all of it within
# seconds of the heartbeat stopping, unattended.
#
# It is read-only: it samples and reads logs, and never signals or kills Emacs.
#
# Usage:
#   ./scripts/freeze_watchdog.sh              # run in the foreground
#   nohup ./scripts/freeze_watchdog.sh &      # or leave it running detached
#
# Env overrides:
#   PS_FREEZE_LOG        heartbeat log (default ~/.emacs.d/tmp/ps-freeze.log)
#   PS_FREEZE_MARKER     op marker     (default ~/.emacs.d/tmp/ps-freeze-current-op.txt)
#   PS_FREEZE_CAPTURE_DIR  where captures go (default ~/.emacs.d/tmp/freeze-captures)
#   PS_FREEZE_POLL       seconds between checks (default 10)
#   PS_FREEZE_THRESHOLD  seconds of heartbeat silence that means "frozen" (default 20)
#   PS_FREEZE_PID        watch this pid instead of auto-detecting (for testing)

set -uo pipefail

LOG="${PS_FREEZE_LOG:-$HOME/.emacs.d/tmp/ps-freeze.log}"
MARKER="${PS_FREEZE_MARKER:-$HOME/.emacs.d/tmp/ps-freeze-current-op.txt}"
OUTDIR="${PS_FREEZE_CAPTURE_DIR:-$HOME/.emacs.d/tmp/freeze-captures}"
POLL="${PS_FREEZE_POLL:-10}"
THRESHOLD="${PS_FREEZE_THRESHOLD:-20}"

mkdir -p "$OUTDIR"

# The heartbeat is written every `ps/freeze-log-heartbeat-interval' seconds, so
# the log's mtime is a direct liveness signal for Emacs's main thread.
log_age() {
  local mtime
  mtime=$(stat -f %m "$LOG" 2>/dev/null) || return 1
  echo $(( $(date +%s) - mtime ))
}

# The production Emacs, excluding any sandbox instance from run_emacs_dev.sh.
# PS_FREEZE_PID overrides the search, so this can be pointed at a throwaway
# instance for testing without touching the real session.
emacs_pid() {
  if [ -n "${PS_FREEZE_PID:-}" ]; then
    kill -0 "$PS_FREEZE_PID" 2>/dev/null && echo "$PS_FREEZE_PID"
    return
  fi
  pgrep -f "MacOS/Emacs" 2>/dev/null | while read -r p; do
    if ! ps -o command= -p "$p" | grep -q -- "--init-directory"; then
      echo "$p"; return
    fi
  done
}

capture() {
  local pid="$1" stamp dir
  stamp=$(date +%Y%m%d-%H%M%S)
  dir="$OUTDIR/$stamp"
  mkdir -p "$dir"

  echo "[watchdog] freeze detected; capturing to $dir"

  # 1. Native stack -- which C frame the main thread is wedged in.  This is the
  #    single most valuable artifact: it distinguishes the ns_flush_display
  #    wedge (0% CPU) from an eat/GC runaway (100% CPU).
  sample "$pid" 5 -f "$dir/sample.txt" >/dev/null 2>&1

  # 2. Process state, incl. %CPU, which tells the two freeze types apart.
  ps -o pid,ppid,%cpu,%mem,state,etime,time,command -p "$pid" > "$dir/ps.txt" 2>&1
  top -l 2 -pid "$pid" -stats pid,cpu,state,th,csw > "$dir/top.txt" 2>&1

  # 3. In-flight bracketed operation (empty = wedged outside our instrumentation)
  #    and the tail of the heartbeat log, which dates the wedge precisely.
  cp "$MARKER" "$dir/marker.txt" 2>/dev/null || echo "(no marker)" > "$dir/marker.txt"
  tail -400 "$LOG" > "$dir/log-tail.txt" 2>/dev/null

  # 4. Emacs's own child processes -- catches a hung helper as an alternative
  #    explanation before blaming redisplay.
  ps -ef | awk -v p="$pid" '$3==p' > "$dir/children.txt" 2>&1

  # 5. What macOS thought was happening.  Nothing has shown up here so far
  #    (the app looks alive to the window server while inside [NSApp run]),
  #    which is itself corroborating evidence worth recording each time.
  log show --last 3m \
    --predicate 'processImagePath CONTAINS "Emacs" OR eventMessage CONTAINS[c] "emacs"' \
    --style compact > "$dir/system-log.txt" 2>&1

  # 6. Open windows/frames per the window server, to test the frame-count
  #    hypothesis independently of Emacs's own (now unreachable) state.
  osascript -e 'tell application "System Events" to get name of every window of (every process whose name contains "Emacs")' \
    > "$dir/windows.txt" 2>&1

  {
    echo "captured:  $(date)"
    echo "pid:       $pid"
    echo "log age:   $(log_age)s (heartbeat silent this long)"
    echo "marker:    $(cat "$dir/marker.txt" 2>/dev/null)"
    echo "last beat: $(grep heartbeat "$LOG" 2>/dev/null | tail -1)"
  } > "$dir/summary.txt"

  cat "$dir/summary.txt"
  osascript -e 'display notification "Emacs freeze captured" with title "freeze_watchdog"' 2>/dev/null || true
}

echo "[watchdog] watching $LOG (poll ${POLL}s, threshold ${THRESHOLD}s)"
captured=0
while true; do
  t0=$(date +%s)
  sleep "$POLL"
  t1=$(date +%s)

  # A wall-clock jump far bigger than the poll interval means the machine slept;
  # heartbeats legitimately stop then, so skip this round rather than reporting
  # a false freeze on wake.
  if [ $(( t1 - t0 )) -gt $(( POLL * 3 )) ]; then
    echo "[watchdog] sleep/wake detected, resetting"
    captured=0
    continue
  fi

  age=$(log_age) || continue
  pid=$(emacs_pid)
  [ -z "$pid" ] && { captured=0; continue; }

  if [ "$age" -gt "$THRESHOLD" ]; then
    if [ "$captured" -eq 0 ]; then
      capture "$pid"
      captured=1        # one capture per freeze, not one per poll
    fi
  else
    captured=0          # heartbeat resumed (Emacs restarted): re-arm
  fi
done
