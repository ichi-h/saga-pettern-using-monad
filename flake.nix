{
  description = "scala-develop";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { allowUnfree = true; };
        overlays = [
          (final: prev: {

            overlay-temurin-bin = prev.temurin-bin.overrideAttrs (oldAttrs: {
              version = "21.0.9";
              src = prev.fetchurl {
                # See: https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html#scala-compatibility-table
                # See: https://github.com/adoptium/temurin21-binaries/releases
                url = "https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.9%2B10/OpenJDK21U-jdk_x64_linux_hotspot_21.0.9_10.tar.gz";
                hash = "sha256-gQ03c99+DWxDlOTiRLJkyLMOCwWgrPVC0GX9eKa2XC8=";
              };
            });

            overlay-scala = prev.scala.bare.overrideAttrs (oldAttrs: {
              version = "3.3.7";
              src = prev.fetchurl {
                # See: https://github.com/scala/scala3/releases/tag/3.3.7
                url = "https://github.com/scala/scala3/releases/download/3.3.7/scala3-3.3.7.tar.gz";
                hash = "sha256-7OUQ4jmLILwxQit9gVrDROlorJ5xyWRF218TbNKYxNk=";
              };
              propagatedBuildInputs = [ ]; # Do not install JRE
              preFixup = ""; # Do not set JAVA_HOME
            });

            overlay-sbt = prev.sbt.overrideAttrs (oldAttrs: {
              version = "1.12.0";
              src = prev.fetchurl {
                # See: https://github.com/sbt/sbt/releases
                url = "https://github.com/sbt/sbt/releases/download/v1.12.0/sbt-1.12.0.tgz";
                hash = "sha256-5K3j9L2+HnRFvprPAssbNXS+tGA2LhcofiRrfq+buVI=";
              };
              postPatch = ""; # Do not set JAVA_HOME
            });

          })
        ];
        pkgs = import nixpkgs { inherit config overlays system; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            overlay-sbt
            overlay-scala
            overlay-temurin-bin
          ];
          env = {
            JAVA_HOME = "${pkgs.overlay-temurin-bin}";
          };
        };
      }
    );
}
