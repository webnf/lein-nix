with import <nixpkgs> {};
let

  lein-jar = "${leiningen}/share/java/${leiningen.name}-standalone.jar";

in {
  classpathFor = prj-dir: opts:
    import (runCommand "lein-nix-classpath" ({
      buildInputs = [ leiningen ];
#      LEIN_HOME = writeTextDir "profiles.clj" ''
#        {:nix {:source-paths [ "${./src}" ]
#               :eval-in-leiningen true}}
#      '';
      profiles = [ ];
    } // opts) ''
      export LEIN_HOME="`pwd`/home"
      mkdir -p "$LEIN_HOME"
      cp -a ${prj-dir} tmp-project && chmod -R +w tmp-project
      cj () {
        local IFS=","
        echo "$*"
      }
      if [ -n "$profiles" ]; then
        profile_opts="with-profiles $(cj $profiles)"
      fi
      export profile_opts
      (cd tmp-project
       echo "${./src}/" > .lein-classpath
       exec lein $profile_opts nix build-classpath $out)
    '');
    ##  exec java -cp ${./src}/:${lein-jar} clojure.main -m leiningen.nix gen-classpath $out
    ##  lein $profile_opts nix build-classpath $out
    
}
