{ stdenv, jre, lib, writeText }:

let
  desc = import ./descriptor.nix;
  # inherit (desc) initForm version classpath;
in stdenv.mkDerivation {
  name = "${desc.name}-${desc.version}";
  buildCommand = ''
    mkdir -p $out/bin $out/share
    cp ${writeText "${desc.name}-classpath" (lib.concatStringsSep "\n" (import ./classpath.nix))} $out/share/classpath
    cp ${writeText "${desc.name}-version"   desc.version} $out/share/version
    cp ${writeText "${desc.name}-init.clj"  desc.initForm} $out/share/init.clj
    classpath="$out/share/classpath"
    initClj="$out/share/init.clj"
    java="${jre}/bin/java"
    javaArgs='${lib.concatStringsSep " " desc.javaArgs}'
    shell="${stdenv.shell}"
    substituteAll ${./start.tmpl.sh} $out/bin/run-${desc.name}
    chmod +x $out/bin/run-${desc.name}
  '';
}
