#!/usr/bin/env bash

ranges=(0.0.0.0-9.255.255.255
        11.0.0.0-172.15.255.255
        172.32.0.0-192.167.255.255
        192.169.0.0-223.255.255.255)

nets() {
    for range in "${ranges[@]}"; do
        ipcalc "$range" | grep -v deaggregate
    done
}

nets | xargs -l1 ipcalc | perl -naE 'if (/Address:/) { print "\"\\\"route " . $F[1] } if (/Netmask:/) { say " " . $F[1] . "\\\"\""}'
