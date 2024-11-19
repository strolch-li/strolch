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

package li.strolch.privilege.base;

public class PrivilegeConstants {

	public static final String DEFAULT_ALGORITHM_NON_SALT = "SHA-256";
	public static final String DEFAULT_ALGORITHM = "PBKDF2WithHmacSHA512";
	public static final int DEFAULT_KEY_LENGTH = 256;
	public static final int DEFAULT_SMALL_ITERATIONS = 10000;
	public static final int DEFAULT_ITERATIONS = 200000;

	public static final String REALM = "realm";
	public static final String ORGANISATION = "organisation";
	public static final String LOCATION = "location";
	public static final String LOCATIONS = "locations";
	public static final String PRIMARY_LOCATION = "primaryLocation";
	public static final String SECONDARY_LOCATIONS = "secondaryLocations";
	public static final String ROLES = "roles";
	public static final String GROUPS = "groups";
	public static final String EMAIL = "email";
	public static final String VALID_FROM = "validFrom";
	public static final String VALID_TO = "validTo";

	public static final String ROLE_STROLCH_ADMIN = "StrolchAdmin";
}
