#!/usr/bin/env bash
set -euo pipefail

ROOT=$(readlink -f $(dirname $0))
VM=win10

case "${1:-on}" in
    off)
        OP=detach-device
        ;;
    *)
        OP=attach-device
        ;;
esac

for dev_xml in $ROOT/*.xml; do
    echo $OP $dev_xml
    sudo virsh $OP $VM $dev_xml || true
done
