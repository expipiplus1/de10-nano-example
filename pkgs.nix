let pkgsRev = "26379b5be8e8aaade912794939a209c72d2b980e";
    pkgsSHA256 = "0ndf3v2yyvxbz3xgmpb7ska5iqzkf7m26021nzv0x4a4yzjbdnv8";
in builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${pkgsRev}.tar.gz";
  sha256 = pkgsSHA256;
}
