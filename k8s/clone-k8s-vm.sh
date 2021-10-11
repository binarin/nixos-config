#!/usr/bin/env bash

JUMPHOST=root@192.168.2.25
IP_BASE=32

for idx in 1 2 3; do
    NAME=k0s-$idx
    VMID=100$idx
    IP=192.168.2.$(($IP_BASE + $idx))
    ssh-keygen -R $IP

    ssh $JUMPHOST qm clone 9000 $VMID --name $NAME
    ssh $JUMPHOST qm resize $VMID scsi0 +500G
    ssh $JUMPHOST qm set $VMID \
        -balloon 2048 \
        -memory 8192 \
        -cores 4 \
        -ciuser binarin \
        -ipconfig0 ip=$IP/24,gw=192.168.2.1 \
        -sshkeys /root/.ssh/authorized_keys
    ssh $JUMPHOST qm start $VMID
done
