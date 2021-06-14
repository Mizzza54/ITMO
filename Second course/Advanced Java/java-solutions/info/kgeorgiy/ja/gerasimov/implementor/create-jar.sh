#!/bin/bash
#Start location: /Users/michael/Desktop/java-advanced-2021/modules/info.kgeorgiy.ja.gerasimov/info/kgeorgiy/ja/gerasimov/implementor
pathToRoot="../../../../../../../../java-advanced-2021/" #Location: /Users/michael/Desktop/java-advanced-2021
pathToArtifacts="$pathToRoot/artifacts/"
pathToBuild="./_jar"
if [ -d $pathToBuild ]; then
        rm -r -f $pathToBuild
fi
mkdir -p "$pathToBuild"
javac -verbose -d "$pathToBuild/" -cp "$pathToArtifacts/info.kgeorgiy.java.advanced.implementor.jar" Implementor.java
jar cfm JarImplementor.jar ./MANIFEST.MF -C ./$pathToBuild .
rm -r -f $pathToBuild
