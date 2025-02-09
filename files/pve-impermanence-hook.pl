#!/usr/bin/perl
use strict;
use warnings;

print "GUEST HOOK: " . join(' ', @ARGV). "\n";

# First argument is the vmid
my $vmid = shift;

# Second argument is the phase
my $phase = shift;

if ($phase eq 'pre-start') {
    # First phase 'pre-start' will be executed before the guest
    # is started. Exiting with a code != 0 will abort the start
    print "$vmid is starting, doing preparations.\n";

    print "zfs rollback -r rpool/data/subvol-$vmid-disk-0\@blank\n";
    system("zfs rollback -r rpool/data/subvol-$vmid-disk-0\@blank") == 0
      or die "zfs rollback failed: $?";

    # print "preparations failed, aborting."
    # exit(1);
} elsif ($phase eq 'post-start') {
    # Second phase 'post-start' will be executed after the guest
    # successfully started.

    print "$vmid started successfully.\n";
} elsif ($phase eq 'pre-stop') {
    # Third phase 'pre-stop' will be executed before stopping the guest
    # via the API. Will not be executed if the guest is stopped from
    # within e.g., with a 'poweroff'
    print "$vmid will be stopped.\n";
} elsif ($phase eq 'post-stop') {
    # Last phase 'post-stop' will be executed after the guest stopped.
    # This should even be executed in case the guest crashes or stopped
    # unexpectedly.
    print "$vmid stopped. Doing cleanup.\n";

} else {
    die "got unknown phase '$phase'\n";
}

exit(0);
