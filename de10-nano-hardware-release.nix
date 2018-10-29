{ pkgs }:

pkgs.fetchzip {
  url = "https://github.com/intel/de10-nano-hardware/releases/download/RELEASE-20180612_19.29.23/de10-nano-build_20180612.tgz";
  sha256 = "1mmvjvc8vrfv1j681814nxlxn0d9hgil1id3n1bmri7gsinajjpd";
  stripRoot = false;
}
