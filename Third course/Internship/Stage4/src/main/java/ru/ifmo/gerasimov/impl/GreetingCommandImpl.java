package ru.ifmo.gerasimov.impl;

import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import ru.ifmo.gerasimov.GreetingCommand;

/**
 * @author Michael Gerasimov
 */
@Component(
        service = GreetingCommand.class,
        immediate = true,
        property = {
                "osgi.command.scope=practice",
                "osgi.command.function=hello"
        }
)
public class GreetingCommandImpl implements GreetingCommand {

    @Activate
    public GreetingCommandImpl() {
        System.out.println("Hello, Gogo Shell");
    }

    public String hello() {
        return "Hello,";
    }

    @Override
    public String hello(String str) {
        return String.format("Hello, %s%n", str);
    }

    @Deactivate
    public void deactivate() {
        System.out.println("Goodbye, Gogo Shell");
    }
}
