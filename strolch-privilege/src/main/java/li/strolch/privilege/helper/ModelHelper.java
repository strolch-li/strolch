package li.strolch.privilege.helper;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static java.lang.String.join;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

public class ModelHelper {

	public static Map<String, String> buildLocationProperties(String realm, Set<String> organisations,
			Set<String> locations, String primaryLocation, Set<String> secondaryLocations) {
		Map<String, String> properties = new HashMap<>();
		if (isNotEmpty(realm))
			properties.put(REALM, realm);
		if (!organisations.isEmpty())
			properties.put(ORGANISATION, join(",", organisations));
		if (!locations.isEmpty())
			properties.put(LOCATION, join(",", locations));
		if (isNotEmpty(primaryLocation))
			properties.put(PRIMARY_LOCATION, primaryLocation);
		if (!secondaryLocations.isEmpty())
			properties.put(SECONDARY_LOCATIONS, join(",", secondaryLocations));
		return properties;
	}
}
