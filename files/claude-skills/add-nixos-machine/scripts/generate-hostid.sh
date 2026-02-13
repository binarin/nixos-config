#!/usr/bin/env bash
# Generate a random hostId for NixOS machines
# Output: 8-character lowercase hex string
head -c4 /dev/urandom | od -A none -t x4 | perl -nE 'm,(\w+), && print $1'
