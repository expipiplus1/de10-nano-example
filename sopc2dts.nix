{ pkgs }:

with pkgs;

stdenv.mkDerivation rec {
  name = "sopc2dts-${version}";
  version = "2018-06-12";

  src = fetchFromGitHub {
    owner = "wgoossens";
    repo = "sopc2dts";
    rev = "2be4c1bf413016899445f5eaf7a2bcbaf76b17bc";
    sha256 = "0q548f0s0f0zqdwskbs3vyi1b553cjb6d2vncd0r5l997nnlxhxv";
  };

  patches =
    let intel-de10-nano = fetchFromGitHub {
          owner = "intel";
          repo = "de10-nano-hardware";
          rev = "138080dc40b53954dc1f2806cde53a6393c300e2";
          sha256 = "1nm0wvp7n7xsp38bpwgqcbjha388954m6j38xliizcq232kdwfaj";
        };
    in  [ (intel-de10-nano + "/patches/sopc2dts/0001-Modifications-for-dts-overlay-support.patch")
          (intel-de10-nano + "/patches/sopc2dts/0002-Use-target-path-instead-of-phandle.patch")
          (intel-de10-nano + "/patches/sopc2dts/0003-remove-since-we-moved-to-full-path-for-overlay-terge.patch")
          ./sopc2dts_components.patch
        ];

  nativeBuildInputs = [ jdk makeWrapper ];

  installPhase = ''
    mkdir -p "$out/bin"
    mkdir -p "$out/share/java"
    mv sopc2dts.jar "$out/share/java/"
    makeWrapper ${jdk}/bin/java "$out/bin/sopc2dts" --add-flags "-jar $out/share/java/sopc2dts.jar" --suffix PATH : ${stdenv.lib.makeBinPath [ jdk ]}
  '';

  meta = with stdenv.lib; {
    description = "Tool to create a devicetree from a sopcinfo file generated by Altera";
    longDescription = ''
      Tool to create a devicetree from a sopcinfo file generated by Altera.

      More info available at: http://www.alterawiki.com/wiki/Sopc2dts
      http://rocketboards.org/foswiki/Documentation/DeviceTreeGenerator140
    '';
    homepage = https://github.com/wgoossens/sopc2dts;
    license = licenses.gpl2;
    maintainers = [ maintainers.expipiplus1 ];
    platforms = platforms.all;
  };
}
