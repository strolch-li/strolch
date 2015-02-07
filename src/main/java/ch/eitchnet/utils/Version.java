package ch.eitchnet.utils;

import java.util.NoSuchElementException;
import java.util.StringTokenizer;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * This class has been adapted from org.osgi.framework.Version
 * 
 * Version identifier.
 * 
 * <p>
 * Version identifiers have four components.
 * <ol>
 * <li>Major version. A non-negative integer.</li>
 * <li>Minor version. A non-negative integer.</li>
 * <li>Micro version. A non-negative integer.</li>
 * <li>Qualifier. A text string. See {@code Version(String)} for the format of the qualifier string.</li>
 * </ol>
 * 
 * <b>Note:</b> The qualifier can be separated by two different styles: {@link #OSGI_QUALIFIER_SEPARATOR} or
 * {@link #MAVEN_QUALIFIER_SEPARATOR}. Thus the qualifier my also have two special values:
 * {@link #OSGI_SNAPSHOT_QUALIFIER} or {@value #MAVEN_SNAPSHOT_QUALIFIER}.
 * 
 * <p>
 * The grammar for parsing version strings is as follows:
 * 
 * <pre>
 * version ::= major('.'minor('.'micro('.'qualifier)?)?)?
 * major ::= digit+
 * minor ::= digit+
 * micro ::= digit+
 * qualifier ::= (alpha|digit|'_'|'-')+
 * digit ::= [0..9]
 * alpha ::= [a..zA..Z]
 * </pre>
 * 
 * <b>Note:</b> There must be no whitespace in version.
 * </p>
 * 
 * <p>
 * <b>Note:</b> {@code Version} objects are immutable and thus thread safe
 * </p>
 */
public class Version implements Comparable<Version> {

	public static final String SEPARATOR = ".";
	public static final String OSGI_QUALIFIER_SEPARATOR = ".";
	public static final String MAVEN_QUALIFIER_SEPARATOR = "-";

	public static final String MAVEN_SNAPSHOT_QUALIFIER = "SNAPSHOT";
	public static final String OSGI_SNAPSHOT_QUALIFIER = "qualifier";

	private final int major;
	private final int minor;
	private final int micro;
	private final String qualifier;

	private transient String versionString;
	private boolean osgiStyle;

	/**
	 * The empty version "0.0.0".
	 */
	public static final Version emptyVersion = new Version(0, 0, 0);

	/**
	 * Creates a version identifier from the specified numerical components. This instance will have
	 * {@link #isOsgiStyle()} return false
	 * 
	 * <p>
	 * The qualifier is set to the empty string.
	 * 
	 * @param major
	 *            Major component of the version identifier.
	 * @param minor
	 *            Minor component of the version identifier.
	 * @param micro
	 *            Micro component of the version identifier.
	 * @throws IllegalArgumentException
	 *             If the numerical components are negative.
	 */
	public Version(final int major, final int minor, final int micro) {
		this(major, minor, micro, null);
	}

	/**
	 * Creates a version identifier from the specified components. This instance will have {@link #isOsgiStyle()} return
	 * false
	 * 
	 * @param major
	 *            Major component of the version identifier.
	 * @param minor
	 *            Minor component of the version identifier.
	 * @param micro
	 *            Micro component of the version identifier.
	 * @param qualifier
	 *            Qualifier component of the version identifier. If {@code null} is specified, then the qualifier will
	 *            be set to the empty string.
	 * 
	 * @throws IllegalArgumentException
	 *             If the numerical components are negative or the qualifier string is invalid.
	 */
	public Version(final int major, final int minor, final int micro, String qualifier) {
		this(major, minor, micro, null, false);
	}

	/**
	 * Creates a version identifier from the specified components.
	 * 
	 * @param major
	 *            Major component of the version identifier.
	 * @param minor
	 *            Minor component of the version identifier.
	 * @param micro
	 *            Micro component of the version identifier.
	 * @param qualifier
	 *            Qualifier component of the version identifier. If {@code null} is specified, then the qualifier will
	 *            be set to the empty string.
	 * @param osgiStyle
	 *            if true, then this is an osgi style version, otherwise not
	 * 
	 * @throws IllegalArgumentException
	 *             If the numerical components are negative or the qualifier string is invalid.
	 */
	public Version(final int major, final int minor, final int micro, String qualifier, boolean osgiStyle) {
		if (qualifier == null) {
			qualifier = "";
		}

		this.major = major;
		this.minor = minor;
		this.micro = micro;
		this.qualifier = qualifier;
		this.versionString = null;
		validate();
	}

	/**
	 * <p>
	 * Creates a version identifier from the specified string.
	 * </p>
	 * 
	 * @param version
	 *            String representation of the version identifier.
	 * 
	 * @throws IllegalArgumentException
	 *             If {@code version} is improperly formatted.
	 */
	private Version(final String version) {
		int maj = 0;
		int min = 0;
		int mic = 0;
		String qual = StringHelper.EMPTY;

		try {
			StringTokenizer st = new StringTokenizer(version, SEPARATOR + MAVEN_QUALIFIER_SEPARATOR
					+ OSGI_QUALIFIER_SEPARATOR, true);
			maj = Integer.parseInt(st.nextToken());

			if (st.hasMoreTokens()) { // minor
				st.nextToken(); // consume delimiter
				min = Integer.parseInt(st.nextToken());

				if (st.hasMoreTokens()) { // micro
					st.nextToken(); // consume delimiter
					mic = Integer.parseInt(st.nextToken());

					if (st.hasMoreTokens()) { // qualifier

						String qualifierSeparator = st.nextToken(); // consume delimiter
						this.osgiStyle = qualifierSeparator.equals(OSGI_QUALIFIER_SEPARATOR);

						qual = st.nextToken(StringHelper.EMPTY); // remaining string

						if (st.hasMoreTokens()) { // fail safe
							throw new IllegalArgumentException("invalid format: " + version);
						}
					}
				}
			}
		} catch (NoSuchElementException e) {
			IllegalArgumentException iae = new IllegalArgumentException("invalid format: " + version);
			iae.initCause(e);
			throw iae;
		}

		this.major = maj;
		this.minor = min;
		this.micro = mic;
		this.qualifier = qual;
		this.versionString = null;
		validate();
	}

	/**
	 * Called by the Version constructors to validate the version components.
	 * 
	 * @throws IllegalArgumentException
	 *             If the numerical components are negative or the qualifier string is invalid.
	 */
	private void validate() {
		if (this.major < 0) {
			throw new IllegalArgumentException("negative major");
		}
		if (this.minor < 0) {
			throw new IllegalArgumentException("negative minor");
		}
		if (this.micro < 0) {
			throw new IllegalArgumentException("negative micro");
		}
		char[] chars = this.qualifier.toCharArray();
		for (char ch : chars) {
			if (('A' <= ch) && (ch <= 'Z')) {
				continue;
			}
			if (('a' <= ch) && (ch <= 'z')) {
				continue;
			}
			if (('0' <= ch) && (ch <= '9')) {
				continue;
			}
			if ((ch == '_') || (ch == '-')) {
				continue;
			}
			throw new IllegalArgumentException("invalid qualifier: " + this.qualifier);
		}
	}

	public Boolean isFullyQualified() {
		return !this.qualifier.isEmpty();
	}

	/**
	 * Parses a version identifier from the specified string.
	 * 
	 * <p>
	 * See {@code Version(String)} for the format of the version string.
	 * 
	 * @param version
	 *            String representation of the version identifier. Leading and trailing whitespace will be ignored.
	 * 
	 * @return A {@code Version} object representing the version identifier. If {@code version} is {@code null} or the
	 *         empty string then {@code emptyVersion} will be returned.
	 * 
	 * @throws IllegalArgumentException
	 *             If {@code version} is improperly formatted.
	 */
	public static Version valueOf(String version) {
		if (version == null) {
			return emptyVersion;
		}

		version = version.trim();
		if (version.length() == 0) {
			return emptyVersion;
		}

		return new Version(version);
	}

	/**
	 * Returns true if the given version string can be parsed, meaning a {@link Version} instance can be instantiated
	 * with it
	 * 
	 * @param version
	 *            String representation of the version identifier. Leading and trailing whitespace will be ignored.
	 * 
	 * @return true if no parse errors occurr
	 */
	public static boolean isParseable(String version) {
		try {
			valueOf(version);
			return true;
		} catch (IllegalArgumentException e) {
			return false;
		}
	}

	/**
	 * Returns the major component of this version identifier.
	 * 
	 * @return The major component.
	 */
	public int getMajor() {
		return this.major;
	}

	/**
	 * Returns the minor component of this version identifier.
	 * 
	 * @return The minor component.
	 */
	public int getMinor() {
		return this.minor;
	}

	/**
	 * Returns the micro component of this version identifier.
	 * 
	 * @return The micro component.
	 */
	public int getMicro() {
		return this.micro;
	}

	/**
	 * Returns the qualifier component of this version identifier.
	 * 
	 * @return The qualifier component.
	 */
	public String getQualifier() {
		return this.qualifier;
	}

	/**
	 * Returns a new {@link Version} where each version number is incremented or decreased by the given parameters
	 * 
	 * @param major
	 *            the value to increase or decrease the major part of the version
	 * @param minor
	 *            the value to increase or decrease the minor part of the version
	 * @param micro
	 *            the value to increase or decrease the micro part of the version
	 * 
	 * @return the new Version with the version parts modified as passed in by the parameters
	 */
	public Version add(int major, int minor, int micro) {
		return new Version(this.major + major, this.minor + minor, this.micro + micro, this.qualifier, this.osgiStyle);
	}

	/**
	 * @return true if this is an OSGI style version, i.e. if has a qualifier, then osgi defines how the qualifier is
	 *         appended to the version
	 */
	public boolean isOsgiStyle() {
		return this.osgiStyle;
	}

	/**
	 * @return true if this version is for a snapshot version, i.e. ends with {@link #MAVEN_SNAPSHOT_QUALIFIER} or
	 *         {@link #OSGI_SNAPSHOT_QUALIFIER}
	 */
	public boolean isSnapshot() {
		return MAVEN_SNAPSHOT_QUALIFIER.equals(this.qualifier) || OSGI_SNAPSHOT_QUALIFIER.equals(this.qualifier);
	}

	/**
	 * Returns a hash code value for the object.
	 * 
	 * @return An integer which is a hash code value for this object.
	 */
	@Override
	public int hashCode() {
		return (this.major << 24) + (this.minor << 16) + (this.micro << 8) + this.qualifier.hashCode();
	}

	/**
	 * Compares this {@code Version} object to another object.
	 * 
	 * <p>
	 * A version is considered to be <b>equal to </b> another version if the major, minor and micro components are equal
	 * and the qualifier component is equal (using {@code String.equals}).
	 * 
	 * @param object
	 *            The {@code Version} object to be compared.
	 * @return {@code true} if {@code object} is a {@code Version} and is equal to this object; {@code false} otherwise.
	 */
	@Override
	public boolean equals(final Object object) {
		if (object == this)
			return true;
		if (!(object instanceof Version))
			return false;

		Version other = (Version) object;
		return (this.major == other.major) && (this.minor == other.minor) && (this.micro == other.micro)
				&& this.qualifier.equals(other.qualifier);
	}

	/**
	 * Compares this {@code Version} object to another object ignoring the qualifier part.
	 * 
	 * <p>
	 * A version is considered to be <b>equal to </b> another version if the major, minor and micro components are
	 * equal.
	 * 
	 * @param object
	 *            The {@code Version} object to be compared.
	 * @return {@code true} if {@code object} is a {@code Version} and is equal to this object; {@code false} otherwise.
	 */
	public boolean equalsIgnoreQualifier(final Object object) {
		if (object == this)
			return true;
		if (!(object instanceof Version))
			return false;

		Version other = (Version) object;
		return (this.major == other.major) && (this.minor == other.minor) && (this.micro == other.micro);
	}

	/**
	 * Compares this {@code Version} object to another {@code Version}.
	 * 
	 * <p>
	 * A version is considered to be <b>less than </b> another version if its major component is less than the other
	 * version's major component, or the major components are equal and its minor component is less than the other
	 * version's minor component, or the major and minor components are equal and its micro component is less than the
	 * other version's micro component, or the major, minor and micro components are equal and it's qualifier component
	 * is less than the other version's qualifier component (using {@code String.compareTo}).
	 * 
	 * <p>
	 * A version is considered to be <b>equal to</b> another version if the major, minor and micro components are equal
	 * and the qualifier component is equal (using {@code String.compareTo}).
	 * 
	 * @param other
	 *            The {@code Version} object to be compared.
	 * @return A negative integer, zero, or a positive integer if this version is less than, equal to, or greater than
	 *         the specified {@code Version} object.
	 * @throws ClassCastException
	 *             If the specified object is not a {@code Version} object.
	 */
	@Override
	public int compareTo(final Version other) {
		if (other == this)
			return 0;

		int result = this.major - other.major;
		if (result != 0)
			return result;

		result = this.minor - other.minor;
		if (result != 0)
			return result;

		result = this.micro - other.micro;
		if (result != 0)
			return result;

		return this.qualifier.compareTo(other.qualifier);
	}

	/**
	 * Returns the string representation of this version identifier.
	 * 
	 * <p>
	 * The format of the version string will be {@code major.minor.micro} if qualifier is the empty string or
	 * {@code major.minor.micro.qualifier} otherwise.
	 * 
	 * @return The string representation of this version identifier.
	 */
	@Override
	public String toString() {
		if (this.versionString == null)
			this.versionString = toString(this.osgiStyle);
		return this.versionString;
	}

	private String toString(final boolean withOsgiStyle) {
		int q = this.qualifier.length();
		StringBuilder result = new StringBuilder(20 + q);
		result.append(this.major);
		result.append(SEPARATOR);
		result.append(this.minor);
		result.append(SEPARATOR);
		result.append(this.micro);
		if (q > 0) {
			if (withOsgiStyle) {
				result.append(OSGI_QUALIFIER_SEPARATOR);
			} else {
				result.append(MAVEN_QUALIFIER_SEPARATOR);
			}
			result.append(createQualifier(withOsgiStyle));
		}
		return result.toString();
	}

	private String createQualifier(boolean withOsgiStyle) {
		if (this.qualifier.equals(MAVEN_SNAPSHOT_QUALIFIER) || this.qualifier.equals(OSGI_SNAPSHOT_QUALIFIER)) {
			if (withOsgiStyle) {
				return OSGI_SNAPSHOT_QUALIFIER;
			} else {
				return MAVEN_SNAPSHOT_QUALIFIER;
			}
		}
		return this.qualifier;
	}

	/**
	 * @return This version represented in a maven compatible form.
	 */
	public String toMavenStyleString() {
		return toString(false);
	}

	/**
	 * @return This version represented in an OSGi compatible form.
	 */
	public String toOsgiStyleString() {
		return toString(true);
	}

	/**
	 * @return This only the major and minor version in a string
	 */
	public String toMajorAndMinorString() {
		StringBuilder result = new StringBuilder(20);
		result.append(this.major);
		result.append(SEPARATOR);
		result.append(this.minor);
		return result.toString();
	}
}
