package li.strolch.privilege.helper;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static java.lang.String.join;
import static li.strolch.privilege.base.PrivilegeConstants.*;

public class ModelHelper {

	public static Map<String, String> buildLocationProperties(String realm, Set<String> organisations,
			Set<String> locations, String primaryLocation, Set<String> secondaryLocations) {
		Map<String, String> properties = new HashMap<>();
		properties.put(REALM, realm);
		if (!organisations.isEmpty())
			properties.put(ORGANISATION, join(",", organisations));
		properties.put(LOCATION, join(",", locations));
		properties.put(PRIMARY_LOCATION, primaryLocation);
		properties.put(SECONDARY_LOCATIONS, join(",", secondaryLocations));
		return properties;
	}
}
