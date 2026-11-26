#!/bin/sh
# qm hookscript (args: <vmid> <phase>): on post-start, set the kvm
# process's oom_score_adj from pve-memguard's kill order. Set per VM by
# hand: qm set <vmid> --hookscript local:snippets/oom-priority-hook.sh
exec /usr/bin/python3 -I /usr/local/sbin/pve-oom-priority hook "$@"
