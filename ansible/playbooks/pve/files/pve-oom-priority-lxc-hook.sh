#!/bin/sh
# lxc.hook.start-host for CTs with a positive oom_score_adj, named by
# /etc/lxc/oom-priority/<id>.conf (written by pve-oom-priority sync).
# lxc sets LXC_NAME and LXC_PID (the CT init) and aborts the CT start
# when a hook fails, so this always exits 0. lxc logs hook output at
# DEBUG only, so errors go to syslog.
/usr/bin/python3 -I /usr/local/sbin/pve-oom-priority lxc-start-host "$@" 2>&1 |
  /usr/bin/logger -t pve-oom-priority-lxc-hook || true
exit 0
