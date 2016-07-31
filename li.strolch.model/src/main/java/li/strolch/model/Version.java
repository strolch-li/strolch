package li.strolch.model;

import java.text.MessageFormat;
import java.util.Date;

import li.strolch.utils.dbc.DBC;

/**
 * <p>
 * Defines the version of a {@link StrolchRootElement}. The version of an object allows to store the history of changes
 * as an absolute change, not differential.
 * </p>
 * 
 * <p>
 * Versions have an integer value, which is incremented for each further version. Thus to retrieve the previous version,
 * decrement the objects current version. To find the next version increment the version.
 * </p>
 * 
 * <p>
 * A version has a flag <code>delete</code> which, if true, designates that this version was removed
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Version {

	private final Locator locator;
	private final int version;
	private final String createdBy;
	private final Date createdAt;
	private final boolean deleted;

	/**
	 * Creates a new version instance with the given values. The creation date is now.
	 * 
	 * @param version
	 *            the integer version which must be > 0 and should be incremented for each new version of an object
	 * @param createdBy
	 *            the username of the creator of this object
	 */
	public Version(Locator locator, int version, String createdBy, boolean deleted) {
		DBC.PRE.assertTrue("Version must by > 0", version > 0);
		DBC.PRE.assertNotNull("locator must be set!", locator);
		DBC.PRE.assertNotNull("createdBy must be set!", createdBy);
		this.locator = locator;
		this.version = version;
		this.createdBy = createdBy;
		this.createdAt = new Date();
		this.deleted = deleted;
	}

	public Locator getLocator() {
		return this.locator;
	}

	/**
	 * Returns the integer version, which is > 0
	 * 
	 * @return the version
	 */
	public int getVersion() {
		return this.version;
	}

	/**
	 * Returns the current version incremented
	 * 
	 * @return the current version incremented
	 */
	public int getNextVersion() {
		return this.version + 1;
	}

	/**
	 * Returns true if this version == 0
	 * 
	 * @return true if this version == 0
	 */
	public boolean isFirstVersion() {
		return this.version == 0;
	}

	/**
	 * Returns the current version decremented
	 * 
	 * @return the current version decremented
	 * 
	 * @throws IllegalStateException
	 *             if this version is already the first version
	 */
	public int getPreviousVersion() throws IllegalStateException {
		if (this.version == 0)
			throw new IllegalStateException(
					"This is the first version, no previous version available for " + this.locator);
		return this.version - 1;
	}

	/**
	 * Returns the username of the creator
	 * 
	 * @return the username of the creator
	 */
	public String getCreatedBy() {
		return this.createdBy;
	}

	/**
	 * Returns the date when this version was created
	 * 
	 * @return the date when this version was created
	 */
	public Date getCreatedAt() {
		return this.createdAt;
	}

	/**
	 * Returns true if this version was deleted, otherwise false
	 * 
	 * @return true if this version was deleted, otherwise false
	 */
	public boolean isDeleted() {
		return this.deleted;
	}

	/**
	 * Validates that the given argument is an increment to this version
	 * 
	 * @param other
	 *            the other version to check
	 * 
	 * @throws IllegalArgumentException
	 *             if the given argument's locator is not equal to this version's locator
	 * @throws IllegalStateException
	 *             if the given argument is not the next version
	 */
	public void validateIsNext(Version other) throws IllegalArgumentException, IllegalStateException {
		if (!this.locator.equals(other.locator)) {
			String msg = "Other version {0} is not for same object: {1}";
			throw new IllegalArgumentException(MessageFormat.format(msg, other, this.version));
		}

		if (other.version != this.version + 1) {
			String msg = "Other version: {0} is not an increment to this version: {1}";
			throw new IllegalArgumentException(MessageFormat.format(msg, other, this.version));
		}
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Version [version=");
		builder.append(this.version);
		builder.append(", locator=");
		builder.append(this.locator);
		builder.append(", createdBy=");
		builder.append(this.createdBy);
		builder.append(", createdAt=");
		builder.append(this.createdAt);
		builder.append(", deleted=");
		builder.append(this.deleted);
		builder.append("]");
		return builder.toString();
	}
}
