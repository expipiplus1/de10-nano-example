{ pkgs ? import (import ../pkgs.nix) {}
}:

# Strip out the irrelevant parts of the source
let src = with pkgs.lib;
          cleanSourceWith {
            filter = n: t:
              n != "build" &&
              n != "result" &&
              n != "output_files" &&
              n != "db" &&
              n != "incremental_db" &&
              n != "top" &&
              t != "unknown";
            src = cleanSource ./.;
          };

    clash-compiler = pkgs.fetchFromGitHub{
      owner  = "clash-lang";
      repo = "clash-compiler";
      rev  = "7171fa7f8412ca4145ee00a30b6bf88bdfb23099";
      sha256 = "16j9l53fizai2cs5brybp5jda5wlwm7dmlxy6s3xyydsif9y38qa";
      fetchSubmodules = true;
    };

    haskellPackages = with pkgs.haskell.lib; pkgs.haskell.packages.ghc863.override {
      overrides = self: super: {
        clash-prelude = doJailbreak (dontCheck (self.callCabal2nix "clash-prelude" (clash-compiler + "/clash-prelude") {}));
        clash-ghc = doJailbreak (enableSharedExecutables (self.callCabal2nix "clash-ghc" (clash-compiler + "/clash-ghc") {}));
        clash-lib = self.callCabal2nix "clash-lib" (clash-compiler + "/clash-lib") {};

        ghc-tcplugins-extra = self.callCabal2nix "ghc-tcplugins-extra" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-tcplugins-extra";
          rev  = "ac70960df5b04ec092ea189c8d34b28ab9b41695";
          sha256 = "0ghfndlzw3rdnyxyxjgdbmcnkk985x65wga00ky1acxhlq6md4n4";
        }) {};
        ghc-typelits-extra = self.callCabal2nix "ghc-typelits-extra" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-typelits-extra";
          rev  = "f1cba7cebf73e429dbdfa67c88161300bc5e318e";
          sha256 = "159z5k68yiri75zxp0fxb82clna7c57wll2fwwm17vfhba3780hh";
        }) {};
        ghc-typelits-knownnat = self.callCabal2nix "ghc-typelits-knownnat" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-typelits-knownnat";
          rev  = "7c866bdefff3f8353a29eebb3d35264dacb2af28";
          sha256 = "1s7xf60f9r2i9xhg9p4prm2qw4rvqag0wx1jsrfzrrx8nm3b53rl";
        }) {};
        ghc-typelits-natnormalise = self.callCabal2nix "ghc-typelits-natnormalise" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-typelits-natnormalise";
          rev  = "b4951d4d9b7307154eac0984530bf2d70bca3358";
          sha256 = "06y04gxs21h4pd0bl61flfc4jhzfkkhly5vcm6jkn7pcfxnwflk6";
        }) {};
      };
    };

  haskellBuildInputs = hp: with hp;
    [ clash-ghc
      shake
      singletons
      Earley
      containers
      bifunctors
    ];
  ghcEnv = haskellPackages.ghcWithHoogle haskellBuildInputs;
  # ghcEnv = haskellPackages.ghcWithPackages haskellBuildInputs;
  ghcCommand = "ghc";
  ghcCommandCaps = pkgs.lib.toUpper ghcCommand;
  ghc = haskellPackages.ghc;

  quartus = import ../quartus.nix;
  sopc2dts = import ../sopc2dts.nix { inherit pkgs; };

in

pkgs.stdenv.mkDerivation rec {
  name = "test";
  inherit src;
  nativeBuildInputs = with pkgs;
    [ ghcEnv
      dtc
      sopc2dts
      quartus
    ];
  preBuild = shellHook;
  buildPhase = ''
    export LM_LICENSE_FILE=/home/j/Downloads/7e321a34be7c_1542123969642.dat
    unset SOURCE_DATE_EPOCH
    runhaskell Make.hs -j$NIX_BUILD_CORES
  '';
  installPhase = ''
    mkdir -p "$out"
    cp output_files/test.rbf "$out"
    cp build/test.dtbo "$out"
  '';
  LANG = "en_US.UTF-8";
  shellHook = ''
    export NIX_${ghcCommandCaps}="${ghcEnv}/bin/${ghcCommand}"
    export NIX_${ghcCommandCaps}PKG="${ghcEnv}/bin/ghc-pkg"
    export NIX_${ghcCommandCaps}_DOCDIR="${ghcEnv}/share/doc/ghc/html"
    export NIX_${ghcCommandCaps}_LIBDIR="${ghcEnv}/lib/${ghcCommand}-${ghc.version}"
    # This makes qsys-generate really slow as it's run via proot
    unset SOURCE_DATE_EPOCH
  '';
}
