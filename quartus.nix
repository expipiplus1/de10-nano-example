let
  pkgsPath = import ./pkgs.nix;
  pkgs = import pkgsPath {};
  nixos-config = pkgs.fetchFromGitHub {
    owner = "bjornfor";
    repo = "nixos-config";
    rev = "5fb2d0fd3bfe9e6a8c5ce005e59f440d35d3a268";
    sha256 = "100hbbpczibywyrqcpanbf5sf7b0dlz39dwgwc81v79g0gvn5hn0";
  };
  quartuss = pkgs.callPackage (nixos-config + "/pkgs/altera-quartus") {};
in quartuss.altera-quartus-prime-standard-18
