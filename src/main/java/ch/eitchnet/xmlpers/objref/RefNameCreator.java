package ch.eitchnet.xmlpers.objref;

import ch.eitchnet.utils.helper.StringHelper;

public class RefNameCreator {

	protected static final String SLASH = "/"; //$NON-NLS-1$

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
		return SLASH + realmName + SLASH + type + SLASH + id + SLASH;
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
		return SLASH + realmName + SLASH + type + SLASH + subType + SLASH + id + SLASH;
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
