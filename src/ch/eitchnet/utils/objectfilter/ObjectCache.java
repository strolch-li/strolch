package ch.eitchnet.utils.objectfilter;

import org.apache.log4j.Logger;

/**
 * This class is a cache for objects whose operations (additions, modifications, removals) are first collected and then
 * "deployed" in one go.
 * <p>
 * Thus, this class keeps:
 * <ul>
 * <li>An ID of the object, such that it can be referenced externally.</li.>
 * <li>A key for an object, which keeps the object's type.</li>
 * <li>A reference to the current state of the object</li>
 * <li>An identifier of the operation that needs to be performed on this</li>
 * </ul>
 * </p>
 * 
 * @author Michael Gatto <michael@gatto.ch>
 * 
 * @param <T>
 */
public class ObjectCache<T extends ITransactionObject> {

	private final static Logger logger = Logger.getLogger(ObjectCache.class);

	/**
	 * id The unique ID of this object in this session
	 */
	private final long id;
	/**
	 * key The key defining who's registered for this object's state
	 */
	private final String key;
	/**
	 * object The object that shall be cached
	 */
	private T object;
	/**
	 * operation The operation that has occurred on this object.
	 */
	private Operation operation;

	/**
	 * @param key
	 * @param object
	 * @param operation
	 */
	public ObjectCache(String key, T object, Operation operation) {

		this.id = object.getTransactionID();
		this.key = key;
		this.object = object;
		this.operation = operation;

		if (logger.isDebugEnabled()) {
			logger.debug("Instanciated Cache: ID" + this.id + " / " + key + " OP: " + this.operation + " / "
					+ object.toString());
		}
	}

	/**
	 * Set the new object version of this cache.
	 * 
	 * @param object
	 */
	public void setObject(T object) {
		if (logger.isDebugEnabled()) {
			logger.debug("Updating ID " + this.id + " to value " + object.toString());
		}
		this.object = object;
	}

	/**
	 * Change the operation to execute for this object.
	 * 
	 * @param newOperation
	 */
	public void setOperation(Operation newOperation) {
		if (logger.isDebugEnabled()) {
			logger.debug("Updating Operation of ID " + this.id + " from " + this.operation + " to " + newOperation);
		}
		this.operation = newOperation;
	}

	/**
	 * @return the id
	 */
	public long getId() {
		return id;
	}

	/**
	 * @return the key
	 */
	public String getKey() {
		return key;
	}

	/**
	 * @return the object
	 */
	public T getObject() {
		return object;
	}

	/**
	 * @return the operation
	 */
	public Operation getOperation() {
		return operation;
	}
}
