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

	public static final String ROLE_STROLCH_ADMIN = "StrolchAdmin";
}
