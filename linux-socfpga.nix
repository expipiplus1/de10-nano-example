{ stdenv, fetchFromGitHub, buildPackages, fetchurl, perl, buildLinux, modDirVersionArg ? null, ... } @ args:

with stdenv.lib;

buildLinux (args // rec {
  version = "4.18";

  # modDirVersion needs to be x.y.z, will automatically add .0 if needed
  modDirVersion = if (modDirVersionArg == null) then concatStrings (intersperse "." (take 3 (splitString "." "${version}.0"))) else modDirVersionArg;

  # branchVersion needs to be x.y
  extraMeta.branch = concatStrings (intersperse "." (take 2 (splitString "." version)));

  src = fetchFromGitHub {
    owner = "altera-opensource";
    repo = "linux-socfpga";
    rev = "rel_socfpga-4.18_18.10.02_pr";
    sha256 = "0pw6saial4wsg3znahgx4p5bzyr9608km5ppmf5cfw58x0gqzg91";
  };
} // (args.argsOverride or {}))
