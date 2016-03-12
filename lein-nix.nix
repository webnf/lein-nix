{ writeScript, leiningen
, lein-home ? "$HOME/.lein" }:

writeScript "lein-cmd" ''
  LEIN_HOME="${lein-home}" \
  CLASSPATH=${./src} \
  exec ${leiningen}/bin/lein "$@"
''
