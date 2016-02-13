{ stdenv, writeText, lib }:

let
  desc = import ./descriptor.nix;
in stdenv.mkDerivation {

  name = "${desc.name}-${desc.version}";
  buildCommand = ''
    mkdir -p $out/share
    cp ${writeText "${desc.name}-classpath" (lib.concatStringsSep "\n" (import ./classpath.nix))} $out/share/classpath
    cp ${writeText "${desc.name}-version" desc.version} $out/share/version
    cp ${writeText "${desc.name}-package.edn" desc.package} $out/share/package.edn
  '';

}

