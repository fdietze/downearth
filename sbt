#!/usr/bin/env bash
# Delegate to the sbt launcher provided by the nix devShell (flake.nix).
# It reads project/build.properties and bootstraps the matching sbt version
# into ~/.sbt/boot, same as any normal sbt project.
#
# --java-home is required: without it, sbt's own coursier-based JVM
# management picks a JVM independent of $PATH/$JAVA_HOME (observed: it chose
# a system JDK 21 even with JDK 8 first on PATH). Scala 2.10 + java.nio.Buffer
# code needs to compile against JDK 8, where ByteBuffer/IntBuffer/etc. don't
# yet override position()/limit() with covariant return types (that override
# was added in JDK 9, and makes those calls ambiguous for the 2.10 compiler).
exec sbt --java-home "$JAVA_HOME" "$@"
