package li.strolch.persistence.postgresql;

import java.sql.Connection;

import li.strolch.persistence.api.TransactionResult;

public class ArchivePostgreSqlActivityDao extends PostgreSqlActivityDao {

	public static final String TABLE_NAME = "archive_activities";

	public ArchivePostgreSqlActivityDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
	}

	@Override
	protected String getTableName() {
		return TABLE_NAME;
	}
}
