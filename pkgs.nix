let pkgsRev = "ad5d39aa4a9b2065c3823bfaec80e23b56970343";
    pkgsSHA256 = "02pdrvb50xl3vj3ggas7ml2irjmd600521jl1xf91j5r8nf0n5rn";
in builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${pkgsRev}.tar.gz";
  sha256 = pkgsSHA256;
}
