package ch.eitchnet.xmlpers.objref;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class LockableObject {

	private final Lock lock;

	public LockableObject() {
		this.lock = new ReentrantLock();
	}

	/**
	 * @return
	 * @see java.util.concurrent.locks.Lock#tryLock()
	 */
	public boolean tryLock() {
		return this.lock.tryLock();
	}

	/**
	 * 
	 * @see java.util.concurrent.locks.Lock#unlock()
	 */
	public void unlock() {
		this.lock.unlock();
	}
}
