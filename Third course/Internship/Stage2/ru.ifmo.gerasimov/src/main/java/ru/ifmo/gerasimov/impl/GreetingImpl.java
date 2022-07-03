package ru.ifmo.gerasimov.impl;

import ru.ifmo.gerasimov.Greeting;

/**
 * @author Michael Gerasimov
 */
public class GreetingImpl implements Greeting {

    @Override
    public void sayHello() {
        System.out.println("Hello OSGi world!");
    }
}
