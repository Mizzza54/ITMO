#!/bin/bash
#Start location: /Users/michael/Desktop/java-advanced-2021/modules/info.kgeorgiy.ja.gerasimov/info/kgeorgiy/ja/gerasimov/implementor
pathToRoot="../../../../../../../../java-advanced-2021/" #Location: /Users/michael/Desktop/java-advanced-2021
pathToArtifacts="$pathToRoot/artifacts/"
pathToLib="$pathToRoot/lib/"
pathToModules="$pathToRoot/modules"
pathToBuild="./_javadoc"
link="https://docs.oracle.com/en/java/javase/11/docs/api/"
pathToImplementor="/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/"
javadoc \
    -verbose \
    -private \
    -d $pathToBuild \
    -link $link \
     -cp "$pathToArtifacts/info.kgeorgiy.java.advanced.base.jar:$pathToArtifacts/info.kgeorgiy.java.advanced.implementor.jar:$pathToLib/hamcrest-core-1.3.jar:$pathToLib/junit-4.11.jar:$pathToLib/jsoup-1.8.1.jar:$pathToLib/quickcheck-0.6.jar:" \
    ./Implementor.java \
    "$pathToModules$pathToImplementor/Impler.java" \
    "$pathToModules$pathToImplementor/JarImpler.java" \
    "$pathToModules$pathToImplementor/ImplerException.java" \
    ./package-info.java