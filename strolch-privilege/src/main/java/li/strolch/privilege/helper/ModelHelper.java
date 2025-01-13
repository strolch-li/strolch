/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.handler.PersistenceHandler;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.internal.User;

import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

import static java.lang.String.join;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;
import static li.strolch.utils.iso8601.ISO8601.parseToZdt;

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

	/**
	 * Returns a {@link Stream} of all roles of the given user. This includes the roles referenced by the user's groups
	 *
	 * @param user the user for which to stream the roles
	 *
	 * @return a stream of role names
	 */
	public static Stream<String> streamAllRolesForUser(PersistenceHandler persistenceHandler, User user) {
		return Stream.concat(user.getRoles().stream(), streamAllRolesForGroups(persistenceHandler, user.groups()));
	}

	/**
	 * Returns a {@link Stream} of all roles of the given user. This includes the roles referenced by the user's groups
	 *
	 * @param userRep the user for which to stream the roles
	 *
	 * @return a stream of role names
	 */
	public static Stream<String> streamAllRolesForUser(PersistenceHandler persistenceHandler, UserRep userRep) {
		return Stream.concat(userRep.getRoles().stream(),
				streamAllRolesForGroups(persistenceHandler, userRep.getGroups()));
	}

	public static Stream<String> streamAllRolesForGroups(PersistenceHandler persistenceHandler, Set<String> groups) {
		return streamAllRolesForGroups(groups.stream().map(persistenceHandler::getGroup).filter(Objects::nonNull));
	}

	public static Stream<String> streamAllRolesForGroups(Stream<Group> groups) {
		return groups.filter(Objects::nonNull).filter(ModelHelper::isGroupActive).flatMap(g -> g.roles().stream());
	}

	public static boolean isGroupActive(Group group) {
		ZonedDateTime now = ZonedDateTime.now();
		if (group.hasProperty(VALID_FROM)) {
			if (now.isBefore(parseToZdt(group.getProperty(VALID_FROM))))
				return false;
		}

		if (group.hasProperty(VALID_TO)) {
			if (now.isAfter(parseToZdt(group.getProperty(VALID_TO))))
				return false;
		}

		return true;
	}
}
