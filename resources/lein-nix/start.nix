{ runCommand, jdk, lib, name, writeText
, classpath, javaArgs, clojureArgs
, exports ? {} }:

runCommand name {
  inherit classpath javaArgs clojureArgs;
} ''
    declare -a cp
    cat > $out <<EOF
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "export ${n}=\"${v}\"") exports)}
    exec ${jdk}/bin/java -cp $(echo "$classpath" | tr " " ":") $javaArgs clojure.main $clojureArgs
    EOF
    chmod +x $out
''
