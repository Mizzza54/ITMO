package ru.ifmo.gerasimov.impl;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import ru.ifmo.gerasimov.Greeting;

/**
 * @author Michael Gerasimov
 */
public class Activator implements BundleActivator {
    @Override
    public void start(BundleContext bundleContext) throws Exception {
        bundleContext.registerService(Greeting.class.getName(), new GreetingImpl(), null);
    }

    @Override
    public void stop(BundleContext bundleContext) throws Exception {}
}
