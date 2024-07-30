package li.strolch.persistence.impl;

import li.strolch.persistence.api.ArchiveTransaction;
import li.strolch.persistence.api.DataArchiveHandler;
import li.strolch.persistence.api.StrolchTransaction;

public class InMemoryArchiveTransaction extends ArchiveTransaction {

	protected InMemoryArchiveTransaction(StrolchTransaction tx, DataArchiveHandler archiveHandler) {
		super(tx, archiveHandler);
	}

	@Override
	public void close() throws Exception {
		flush();
	}
}
