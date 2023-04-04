/*
 * Copyright 2013 Michael Gatto <michael@gatto.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.utils.objectfilter;

import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is a cache for objects whose operations (additions, modifications, removals) are first collected and then
 * "deployed" in one go.
 * <p>
 * Thus, this class keeps:
 * </p>
 * <ul>
 * <li>An ID of the object, such that it can be referenced externally.</li>
 * <li>A key for an object, which keeps the object's type.</li>
 * <li>A reference to the current state of the object</li>
 * <li>An identifier of the operation that needs to be performed on this</li>
 * </ul>
 *
 * @author Michael Gatto &lt;michael@gatto.ch&gt;
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ObjectCache {

	private final static Logger logger = LoggerFactory.getLogger(ObjectCache.class);

	/**
	 * UNSET Marker to determine if ids have not been set.
	 */
	public static final long UNSET = 0;

	/**
	 * id The unique ID of this object in this session
	 */
	private final long id;

	/**
	 * key The key defining who's registered for this object's state
	 */
	private final String key;

	/**
	 * operation The operation that has occurred on this object.
	 */
	private Operation operation;

	/**
	 * object The objectKey that shall be cached
	 */
	private final Object objectKey;

	/**
	 * object The object that shall be cached
	 */
	private Object object;

	public ObjectCache(long id, String key, Object objectKey, Object object, Operation operation) {

		this.id = id;
		this.key = key;
		this.objectKey = objectKey;
		this.object = object;
		this.operation = operation;

		if (logger.isDebugEnabled()) {
			String sb = "Instanciated Cache: ID" + this.id + " / " + key + " OP: " + this.operation + " / "
					+ objectKey.toString();
			logger.debug(sb);
		}
	}

	/**
	 * Set the new object version of this cache.
	 *
	 * @param object
	 * 		the object to set
	 */
	public void setObject(Object object) {
		if (logger.isDebugEnabled()) {
			logger.debug(
					MessageFormat.format("Updating ID {0} to value {1}", this.id, object.toString())); //$NON-NLS-1$
		}
		this.object = object;
	}

	/**
	 * Change the operation to execute for this object.
	 *
	 * @param newOperation
	 * 		the operation to set
	 */
	public void setOperation(Operation newOperation) {
		if (logger.isDebugEnabled()) {
			String msg = "Updating Operation of ID {0} from {1} to {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.id, this.operation, newOperation);
			logger.debug(msg);
		}
		this.operation = newOperation;
	}

	public long getId() {
		return this.id;
	}

	public String getKey() {
		return this.key;
	}

	public Operation getOperation() {
		return this.operation;
	}

	public Object getObjectKey() {
		return objectKey;
	}

	public Object getObject() {
		return this.object;
	}
}
