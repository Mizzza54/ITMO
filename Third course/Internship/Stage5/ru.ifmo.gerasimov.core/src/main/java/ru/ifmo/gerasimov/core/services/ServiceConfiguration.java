package ru.ifmo.gerasimov.core.services;

import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.AttributeType;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;

/**
 * @author Michael Gerasimov
 */
@ObjectClassDefinition
public @interface ServiceConfiguration {

    @AttributeDefinition(
            name = "Name",
            description = "Enter the name of news",
            type = AttributeType.STRING
    )
    String getName() default "name";

    @AttributeDefinition(
            name = "RSS Feed URL",
            description = "Enter the URL of news",
            type = AttributeType.STRING
    )
    String getURL() default "url";
}