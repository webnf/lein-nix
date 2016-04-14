{ runCommand, criu
, preloadJob
, preloadUser
, taskName ? "resume" }:

runCommand "criu-wrappers" {
  inherit preloadJob preloadUser criu;
  dummy = builtins.currentTime;
} ''
  set -e
  mkdir -p $out/bin $out/share
  export imagesDir=$out/share
  substituteAll ${./preload.sh.in} preload
  chmod +x preload
  echo "Preloading ..."
  /var/setuid-wrappers/su-sh ./preload
  
  substituteAll ${./resume.sh.in} $out/bin/${taskName}
''
