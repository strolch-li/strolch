/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.xmlpers.objref;

import li.strolch.utils.helper.StringHelper;

public class RefNameCreator {

	protected static final String SLASH = "/"; //$NON-NLS-1$

	// FIXME validate each name part that it is a valid literal for file names...

	public static String createRootName(String realmName) {
		assertRealmName(realmName);
		return SLASH + realmName + SLASH;
	}

	public static String createTypeName(String realmName, String type) {
		assertRealmName(realmName);
		assertType(type);
		return SLASH + realmName + SLASH + type + SLASH;
	}

	public static String createIdOfTypeName(String realmName, String type, String id) {
		assertRealmName(realmName);
		assertType(type);
		assertId(id);
		return SLASH + realmName + SLASH + type + SLASH + id;
	}

	public static String createSubTypeName(String realmName, String type, String subType) {
		assertRealmName(realmName);
		assertType(type);
		assertSubType(subType);
		return SLASH + realmName + SLASH + type + SLASH + subType + SLASH;
	}

	public static String createIdOfSubTypeName(String realmName, String type, String subType, String id) {
		assertRealmName(realmName);
		assertType(type);
		assertSubType(subType);
		assertId(id);
		return SLASH + realmName + SLASH + type + SLASH + subType + SLASH + id;
	}

	private static void assertRealmName(String realmName) {
		if (StringHelper.isEmpty(realmName)) {
			throw new IllegalArgumentException("Realm name may not be empty!"); //$NON-NLS-1$
		}
	}

	private static void assertType(String type) {
		if (StringHelper.isEmpty(type)) {
			throw new IllegalArgumentException("type may not be empty!"); //$NON-NLS-1$
		}
	}

	private static void assertSubType(String subType) {
		if (StringHelper.isEmpty(subType)) {
			throw new IllegalArgumentException("subType may not be empty!"); //$NON-NLS-1$
		}
	}

	private static void assertId(String id) {
		if (StringHelper.isEmpty(id)) {
			throw new IllegalArgumentException("id may not be empty!"); //$NON-NLS-1$
		}
	}
}
