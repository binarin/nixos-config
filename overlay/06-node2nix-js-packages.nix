self: super: {
  js = import ../packages/js {
    pkgs = super;
    nodejs = super.nodejs-6_x;
  };
}
