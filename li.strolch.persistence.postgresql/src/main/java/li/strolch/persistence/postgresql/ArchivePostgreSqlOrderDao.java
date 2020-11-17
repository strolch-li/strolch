package li.strolch.persistence.postgresql;

import java.sql.*;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import li.strolch.model.Order;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;

public class ArchivePostgreSqlOrderDao extends PostgreSqlOrderDao {

	public static final String TABLE_NAME = "archive_orders";
	private boolean descending;

	public ArchivePostgreSqlOrderDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
	}

	@Override
	protected String getTableName() {
		return TABLE_NAME;
	}

	public void setDescending(boolean descending) {
		this.descending = descending;
	}

	@Override
	public List<Order> queryAll(long limit, long offset, String... types) {
		if (types.length == 0)
			return queryAll(limit, offset);

		List<Order> list = new ArrayList<>();

		String ordering = this.descending ? "DESC" : "ASC";
		String sql = "select id, type, asxml from {0} where type = ANY(?) and latest = true order by date " + ordering
				+ " limit {1,number,#} offset {2,number,#}";
		sql = MessageFormat.format(sql, TABLE_NAME, limit, offset);

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {

			Array typesArray = statement.getConnection().createArrayOf("varchar", types);
			statement.setArray(1, typesArray);

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					String type = result.getString("type");
					list.add(parseDbObject(result, id, type));
				}

				return list;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}
}
