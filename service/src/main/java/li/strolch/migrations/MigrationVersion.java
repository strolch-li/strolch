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
public record MigrationVersion(Version dataVersion, Version codeVersion) {

	/**
	 * @return the dataVersion
	 */
	@Override
	public Version dataVersion() {
		return dataVersion;
	}

	/**
	 * @return the codeVersion
	 */
	@Override
	public Version codeVersion() {
		return codeVersion;
	}

}
