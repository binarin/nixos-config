{self, lib}:
{
  homeDir = user: if user == "root" then "/root" else "/home/${user}";
}
