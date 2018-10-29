let pkgsRev = "238b0c1ce9b2e6f80be0822b68e1ee3d946e1b8a";
    pkgsSHA256 = "1craw8ph65pk4ifpvb18ik21g59j8hknhz722nk42wb6x3xq42ix";
in builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${pkgsRev}.tar.gz";
  sha256 = pkgsSHA256;
}
