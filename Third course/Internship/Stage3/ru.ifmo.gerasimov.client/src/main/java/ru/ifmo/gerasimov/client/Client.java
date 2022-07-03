package ru.ifmo.gerasimov.client;

import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import ru.ifmo.gerasimov.Greeting;

/**
 * @author Michael Gerasimov
 */

@Component
public class Client {
    @Reference
    private Greeting greeting;

    @Activate
    public void activate() {
        greeting.sayHello();
    }

    @Deactivate
    public void deactivate() {
        System.out.println("Goodbye OSGi world =( (by Client)");
    }
}
