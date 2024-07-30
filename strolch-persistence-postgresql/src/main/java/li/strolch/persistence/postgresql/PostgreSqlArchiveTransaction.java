package li.strolch.persistence.postgresql;

import li.strolch.persistence.api.ArchiveTransaction;
import li.strolch.persistence.api.StrolchTransaction;

import java.sql.Connection;

public class PostgreSqlArchiveTransaction extends ArchiveTransaction {

	private final Connection connection;

	protected PostgreSqlArchiveTransaction(StrolchTransaction tx, PostgreSqlDataArchiveHandler archiveHandler,
			Connection connection) {
		super(tx, archiveHandler);
		this.connection = connection;
	}

	protected Connection getConnection() {
		return this.connection;
	}

	@Override
	public void close() throws Exception {
		try {
			flush();
			this.connection.commit();
		} catch (Exception e) {
			this.connection.rollback();
			throw e;
		}
	}
}
