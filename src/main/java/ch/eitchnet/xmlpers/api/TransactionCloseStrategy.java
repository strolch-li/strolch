package ch.eitchnet.xmlpers.api;

public enum TransactionCloseStrategy {
	COMMIT() {
		@Override
		public void close(PersistenceTransaction tx) {
			tx.autoCloseableCommit();
		}
	},

	ROLLBACK() {
		@Override
		public void close(PersistenceTransaction tx) {
			tx.autoCloseableRollback();
		}
	};

	public void close(PersistenceTransaction tx) {
		throw new UnsupportedOperationException("Override in enum!"); //$NON-NLS-1$
	}
}