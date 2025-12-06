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
  };
}
