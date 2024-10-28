package li.strolch.persistence.postgresql;

import li.strolch.persistence.api.TransactionResult;

import java.sql.Connection;

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
