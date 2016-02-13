{ writeScript, leiningen
, name
, prjPath
, profiles ? []
, command ? "do clean, dev-classpath"
}:

writeScript "lein-drv-${name}" ''
  #!/bin/sh
  cd "${prjPath}"
  exec ${leiningen}/bin/lein ${
    if 0 < lib.length profiles
    then "with-profiles " + lib.concatStringsSep "," profiles
    else ""
  } ${command}
''
