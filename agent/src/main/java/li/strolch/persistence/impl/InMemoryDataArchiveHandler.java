package li.strolch.persistence.impl;

import java.sql.Connection;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.persistence.api.*;

public class InMemoryDataArchiveHandler extends StrolchComponent implements DataArchiveHandler {

	public InMemoryDataArchiveHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void run(StrolchTransaction tx, BiConsumer<Connection, TransactionResult> runnable) {
		runnable.accept(null, null);
	}

	@Override
	public <T> T runWithResult(StrolchTransaction tx, BiFunction<Connection, TransactionResult, T> runnable) {
		return runnable.apply(null, null);
	}

	@Override
	public Connection getConnection(StrolchTransaction tx) {
		return null;
	}

	@Override
	public OrderDao getOrderDao(Connection connection, TransactionResult txResult) {
		return new InMemoryOrderDao();
	}

	@Override
	public ResourceDao getResourceDao(Connection connection, TransactionResult txResult) {
		return new InMemoryResourceDao();
	}

	@Override
	public ActivityDao getActivityDao(Connection connection, TransactionResult txResult) {
		return new InMemoryActivityDao();
	}
}
