let
  pkgsPath = import ./pkgs.nix;
  pkgs = import pkgsPath {};
  nixos-config = pkgs.fetchFromGitHub {
    owner = "bjornfor";
    repo = "nixos-config";
    rev = "784eb9445bd2c3ac96f363083e996d621f7c1478";
    sha256 = "0q5wpd7yqg32gavhy4c7ka7pn3x672swxf9s7hkq3i4innr4cd5r";
  };
  quartuss = pkgs.callPackage (nixos-config + "/pkgs/altera-quartus") {};
in quartuss.altera-quartus-prime-standard-18
