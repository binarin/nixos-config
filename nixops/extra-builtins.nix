{ exec, ... }: {
  pass = name: exec [./nix-pass.sh name];
  detectEnv = exec [./detect-env.sh];
  hostname = exec  [./hostname.sh];
}
