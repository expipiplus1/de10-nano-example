let
  pkgsPath = import ./pkgs.nix;
  pkgs = import pkgsPath {};
  nixos-config = pkgs.fetchFromGitHub {
    owner = "bjornfor";
    repo = "nixos-config";
    rev = "ad9ccdcd027f3ee11306804f3e95e2ca1d756c8e";
    sha256 = "0cs6725az0lgky7nbl7sdy5ydhrnv83kwkkhyb1vkf76zkygab42";
  };
  quartuss = pkgs.callPackage (nixos-config + "/pkgs/altera-quartus") {};
in quartuss.altera-quartus-prime-standard-18
