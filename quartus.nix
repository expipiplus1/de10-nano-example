let
  pkgsPath = import ./pkgs.nix;
  pkgs = import pkgsPath {};
  nixos-config = pkgs.fetchFromGitHub {
    owner = "bjornfor";
    repo = "nixos-config";
    rev = "e8b8f31931006e9c3de68dc623885e517f9289d7";
    sha256 = "0bp0fxqd12wrvq0wmf1c6n5191jf0cyj7c0afw1vkl7xnr3sprd1";
  };
  quartuss = pkgs.callPackage (nixos-config + "/pkgs/altera-quartus") {};
in quartuss.altera-quartus-prime-standard-18
