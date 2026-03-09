{
  # System users/groups with fixed UIDs/GIDs for Docker volume permissions
  # This provides a centralized place to track UID/GID allocations across all services
  systemUsers = {
    archivebox = {
      uid = 2001;
      gid = 2001;
    };
    karakeep = {
      uid = 2002;
      gid = 2002;
    };
    # UID 1026 matches the existing paperless host for easier volume migration
    # GID 100 is the standard "users" group - use that instead of creating a new group
    paperless = {
      uid = 1026;
      gid = 100; # uses "users" group
    };
  };
}
