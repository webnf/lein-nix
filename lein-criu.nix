{ pkgs ? import <nixpkgs> {}
, lein-home ? "$HOME/.lein" }: let

in pkgs.callPackage ./resources/lein-nix/criu.nix {
  preloadJob = pkgs.callPackage ./resources/lein-nix/start.nix {
    name = "preload-lein";
    classpath = [
      "${pkgs.leiningen}/share/java/leiningen-${pkgs.leiningen.version}-standalone.jar"
      ./src
    ];
    javaArgs = [ "-XX:-UsePerfData" "-server" ];
    clojureArgs = [ "-m" "leiningen.core.main" "nix" "preload" ]; # -
    exports = {
      LEIN_HOME = lein-home;
    };
  };
  preloadUser = "herwig";
  taskName = "lein-criu";
}
