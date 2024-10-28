package li.strolch.persistence.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TransactionThreadLocal extends ThreadLocal<StrolchTransaction> {

	private static final Logger logger = LoggerFactory.getLogger(TransactionThreadLocal.class);

	private static final TransactionThreadLocal instance = new TransactionThreadLocal();

	public static boolean hasTx() {
		return instance.get() != null;
	}

	public static StrolchTransaction getTx() {
		StrolchTransaction tx = instance.get();
		if (tx == null)
			throw new IllegalStateException("No TX available on thread " + Thread.currentThread().getName());
		return tx;
	}

	public static void setTx(StrolchTransaction tx) {
		if (instance.get() != null)
			logger.error("THIS THREAD HAS ALREADY OPENED A TX!");
		else
			instance.set(tx);
	}

	public static void removeTx() {
		instance.remove();
	}
}
