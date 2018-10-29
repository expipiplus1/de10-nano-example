let
  pkgsPath = import ./pkgs.nix;
  pkgs = import pkgsPath {};
  nixos-config = pkgs.fetchFromGitHub {
    owner = "bjornfor";
    repo = "nixos-config";
    rev = "6856d243021a703cf949a1ce58e1ee0f56aad51b";
    sha256 = "1s3w0z3d46qdlcbn4s9bshnq88d8fp4wsvnbhasmn9p6z8428pw1";
  };
  quartuss = pkgs.callPackage (nixos-config + "/pkgs/altera-quartus") {};
in quartuss.altera-quartus-prime-lite-18
