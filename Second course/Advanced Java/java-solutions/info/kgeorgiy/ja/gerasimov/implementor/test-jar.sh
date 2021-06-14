#!/bin/bash

javac ./test/TestClass.java
java -jar JarImplementor.jar info.kgeorgiy.ja.gerasimov.implementor.test.TestClass ./test
java -jar JarImplementor.jar -jar info.kgeorgiy.ja.gerasimov.implementor.test.TestClass ./test/TestJar.jar