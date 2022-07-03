package ru.ifmo.gerasimov.core.services.impl;

import org.osgi.service.component.annotations.*;
import org.osgi.service.metatype.annotations.Designate;
import ru.ifmo.gerasimov.core.exceptions.WrappedException;
import ru.ifmo.gerasimov.core.services.AbstractStatsService;
import ru.ifmo.gerasimov.core.services.ServiceConfiguration;
import ru.ifmo.gerasimov.core.services.StatsService;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Michael Gerasimov
 */
@Component(
        service = StatsService.class,
        immediate = true,
        configurationPolicy = ConfigurationPolicy.REQUIRE
)
@Designate(
        ocd = ServiceConfiguration.class,
        factory = true
)
public class ServiceImpl extends AbstractStatsService implements StatsService {

    private String serviceName;
    private String serviceURL;
    private List<StatsService> configsList;

    @Activate
    @Modified
    protected void activate(final ServiceConfiguration serviceConfiguration) {
        serviceName = serviceConfiguration.getName();
        serviceURL = serviceConfiguration.getURL();
    }

    @Reference(
            service = StatsService.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC
    )
    public void bindStatsService(final StatsService config) {
        if (configsList == null){
            configsList = new ArrayList<>();
        }
        configsList.add(config);

    }

    public void unbindStatsService(final StatsService config) {
        configsList.remove(config);
    }

    @Override
    public String getNewsTitle() {
        return serviceName;
    }

    @Override
    public URL getRSSFeedURL() throws WrappedException {
        try {
            return new URL(serviceURL);
        } catch (MalformedURLException e) {
            throw new WrappedException(e);
        }
    }
}
