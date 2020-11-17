package li.strolch.persistence.postgresql;

import java.sql.Connection;

import li.strolch.persistence.api.TransactionResult;

public class ArchivePostgreSqlResourceDao extends PostgreSqlResourceDao {

	public static final String TABLE_NAME = "archive_resources";

	public ArchivePostgreSqlResourceDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
	}

	@Override
	protected String getTableName() {
		return TABLE_NAME;
	}
}
