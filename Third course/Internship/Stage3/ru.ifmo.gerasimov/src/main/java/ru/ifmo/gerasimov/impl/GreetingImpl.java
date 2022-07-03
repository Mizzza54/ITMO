package ru.ifmo.gerasimov.impl;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import ru.ifmo.gerasimov.Greeting;

/**
 * @author Michael Gerasimov
 */
@Component(service = Greeting.class, immediate = true)
public class GreetingImpl implements Greeting {
    @Override
    public void sayHello() {
        System.out.println("Hello OSGi world!");
    }

    @Deactivate
    public void deactivate() {
        System.out.println("Goodbye OSGi world =( (by Service)");
    }
}