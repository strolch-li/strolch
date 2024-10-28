/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
