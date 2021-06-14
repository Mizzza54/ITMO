#!/bin/bash

# shellcheck disable=SC2034
module="info.kgeorgiy.ja.gerasimov"
#Start location: /Users/michael/Desktop/java-advanced-2021/modules/info.kgeorgiy.ja.gerasimov/info/kgeorgiy/ja/gerasimov/bank
pathToRoot="../../../../../../../../java-advanced-2021/" #Location: /Users/michael/Desktop/java-advanced-2021
pathToLib="$pathToRoot/lib/"

pathToBuild="$pathToRoot/outBank"
if [ -d $pathToBuild ]; then
        rm -r -f $pathToBuild
fi

mkdir -p "$pathToBuild"
javac -verbose -d "$pathToBuild/" -cp "$pathToLib/junit-4.11.jar" ./*.java


#rm -r -f $pathToBuild
