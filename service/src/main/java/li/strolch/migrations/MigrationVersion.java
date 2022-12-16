/**
 *
 */
package li.strolch.migrations;

import li.strolch.utils.Version;

/**
 * Migration versions for data and code migrations
 *
 * @author Reto Breitenmoser <reto.breitenmoser@4trees.ch>
 */
public class MigrationVersion {

	private Version dataVersion;
	private Version codeVersion;

	public MigrationVersion(Version dataVersion, Version codeVersion) {
		this.dataVersion = dataVersion;
		this.codeVersion = codeVersion;
	}

	/**
	 * @return the dataVersion
	 */
	public Version getDataVersion() {
		return dataVersion;
	}

	/**
	 * @return the codeVersion
	 */
	public Version getCodeVersion() {
		return codeVersion;
	}

}
