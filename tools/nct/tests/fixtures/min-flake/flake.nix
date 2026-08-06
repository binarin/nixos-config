{
  description = "minimal fixture flake for nct eval-expr tests (no inputs)";
  outputs = _: {
    # Static values the nct integration tests navigate + apply lambdas to.
    fixtureValue = {
      names = [ "alpha" "beta" "gamma" ];
      nested = { a = 1; b = 2; };
    };
  };
}
