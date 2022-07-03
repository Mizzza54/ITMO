package ru.ifmo.gerasimov.ria;

import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.component.annotations.*;

import ru.ifmo.gerasimov.core.services.AbstractStatsService;
import ru.ifmo.gerasimov.core.services.StatsService;
import ru.ifmo.gerasimov.core.exceptions.WrappedException;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Map;

/**
 * @author Michael Gerasimov
 */
@Component(
        service = StatsService.class,
        immediate = true,
        properties = "OSGI-INF/config.properties"
)
//        configurationPolicy = ConfigurationPolicy.REQUIRE,
//                configurationPid = "RIAService"
public class RIAService extends AbstractStatsService implements StatsService {
    private static final String CONFIGURATION_NAME = "ru.ifmo.gerasimov.ria.RIAService";
    private static final String NAME_PROPERTY = "name";
    private static final String URL_PROPERTY = "url";

    private ConfigurationAdmin configurationAdmin;

    private String serviceName;
    private String serviceURL;

    @Activate
    public RIAService(@Reference final ConfigurationAdmin configurationAdmin, final Map<String, ?> properties) {
        this.configurationAdmin = configurationAdmin;
        try {
            serviceName = (String) properties.get(NAME_PROPERTY);
            serviceURL = (String) properties.get(URL_PROPERTY);
            Configuration config = configurationAdmin.getConfiguration(CONFIGURATION_NAME, null);
            Dictionary<String, Object> props = config.getProperties();

            if (props == null) {
                props = new Hashtable<>();
            }

            props.put(NAME_PROPERTY, serviceName);
            props.put(URL_PROPERTY, serviceURL);

            config.update(props);
        } catch (IOException e) {
            System.err.println("Access to persistent storage fails");
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
