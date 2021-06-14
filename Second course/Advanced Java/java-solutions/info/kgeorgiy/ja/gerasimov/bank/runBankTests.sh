#!/bin/bash

module="info.kgeorgiy.ja.gerasimov"
#Start location: /Users/michael/Desktop/java-advanced-2021/modules/info.kgeorgiy.ja.gerasimov/info/kgeorgiy/ja/gerasimov/bank
pathToRoot="../../../../../../../../java-advanced-2021/" #Location: /Users/michael/Desktop/java-advanced-2021
pathToLib="$pathToRoot/lib/"
pathToBuild="$pathToRoot/outBank"

# shellcheck disable=SC2164
cd $pathToBuild
#-jar "../lib/junit-4.11.jar"
java -cp ../lib/hamcrest-core-1.3.jar:../lib/junit-4.11.jar:. org.junit.runner.JUnitCore "$module.bank.BankTests"
