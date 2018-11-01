{ stdenv, fetchFromGitHub, buildPackages, fetchurl, perl, buildLinux, modDirVersionArg ? null, ... } @ args:

with stdenv.lib;

buildLinux (args // rec {

  # modDirVersion needs to be x.y.z, will automatically add .0 if needed
  modDirVersion = if (modDirVersionArg == null) then concatStrings (intersperse "." (take 3 (splitString "." "${version}.0"))) else modDirVersionArg;

  # branchVersion needs to be x.y
  extraMeta.branch = concatStrings (intersperse "." (take 2 (splitString "." version)));

  # This is the topic/overlays branch rebased on master
  version = "4.19";
  src = fetchFromGitHub {
    owner = "expipiplus1";
    repo = "linux";
    rev = "fb9e07c1987f983097294c14a7bd6b0bf02bbe48";
    sha256 = "06wbzsav1waycm4b68pl2yvss107aj35mvkbr8pd0fyp9d6iazzl";
  };
} // (args.argsOverride or {}))
