package ru.ifmo.gerasimov.client;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import ru.ifmo.gerasimov.Greeting;

/**
 * @author Michael Gerasimov
 */
public class Client implements BundleActivator {

    @Override
    public void start(BundleContext bundleContext) throws Exception {
        ServiceReference<?> ref = bundleContext.getServiceReference(Greeting.class.getName());
        if (ref == null) {
            System.out.println("Dont have");
            return;
        }
        ((Greeting) bundleContext.getService(ref)).sayHello();
    }

    @Override
    public void stop(BundleContext bundleContext) throws Exception {}
}
