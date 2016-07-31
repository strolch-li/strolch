package li.strolch.persistence.postgresql;

import java.sql.SQLXML;

import li.strolch.model.StrolchRootElement;

public abstract class PostgresqlXmlDao<T extends StrolchRootElement> extends PostgresqlDao<T> {

	public PostgresqlXmlDao(PostgreSqlStrolchTransaction tx) {
		super(tx);
	}

	protected abstract T parseFromXml(String id, String type, SQLXML xml);
}
