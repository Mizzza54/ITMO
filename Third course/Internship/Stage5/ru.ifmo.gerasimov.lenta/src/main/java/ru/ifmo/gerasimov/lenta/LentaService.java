package ru.ifmo.gerasimov.lenta;

import org.osgi.service.component.annotations.*;

import ru.ifmo.gerasimov.core.services.AbstractStatsService;
import ru.ifmo.gerasimov.core.services.StatsService;
import ru.ifmo.gerasimov.core.exceptions.WrappedException;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

/**
 * @author Michael Gerasimov
 */
@Component(
        service = StatsService.class,
        immediate = true,
        properties = "OSGI-INF/config.properties"
)
public class LentaService extends AbstractStatsService implements StatsService {
    private static final String NAME_PROPERTY = "name";
    private static final String URL_PROPERTY = "url";

    private String serviceName;
    private String serviceURL;

    @Activate
    protected void activate(final Map<String, ?> properties) {
        try {
            serviceName = (String) properties.get(NAME_PROPERTY);
            serviceURL = (String) properties.get(URL_PROPERTY);
        } catch (ClassCastException | NullPointerException e) {
            System.err.println("Invalid config");
        }
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
