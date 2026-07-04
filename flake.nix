{
  description = "downearth - 3D voxel game in Scala";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # LWJGL 2.9.1's prebuilt native .so (from 2013) dynamically links
        # against these X11 libraries. NixOS has no global /usr/lib, so
        # without these on LD_LIBRARY_PATH the natives fail to load
        # (observed: "libXxf86vm.so.1: cannot open shared object file").
        lwjglNativeLibs = (with pkgs; [
          libx11
          libxext
          libxcursor
          libxrandr
          libxxf86vm
        ]) ++ [
          pkgs.libglvnd  # provides libGL.so.1, dispatches to the system driver at /run/opengl-driver
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          name = "downearth-env";

          buildInputs = with pkgs; [
            jdk8           # Scala 2.10.4 era compatibility for the compiled code
            sbt            # modern sbt launcher; reads project/build.properties for the project sbt version
            bash           # For running ./sbt script
            git
            direnv
          ] ++ lwjglNativeLibs;

          shellHook = ''
            # libjawt.so (JDK's AWT native bridge) is also a runtime dependency
            # of the LWJGL native lib, resolved the same LD_LIBRARY_PATH way.
            # /run/opengl-driver/lib is the NixOS-standard location for the
            # actual (hardware-specific) OpenGL driver that libglvnd dispatches to.
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath lwjglNativeLibs}:${pkgs.jdk8}/lib/openjdk/lib/amd64:/run/opengl-driver/lib:$LD_LIBRARY_PATH
          '';
        };
      }
    );
}
