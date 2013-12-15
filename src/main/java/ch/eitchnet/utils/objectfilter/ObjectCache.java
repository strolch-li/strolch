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
package ch.eitchnet.utils.objectfilter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
 * @author Robert von Burg <eitch@eitchnet.ch>
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
	 * object The object that shall be cached
	 */
	private Object object;

	/**
	 * @param id
	 * @param key
	 * @param object
	 * @param operation
	 */
	public ObjectCache(long id, String key, Object object, Operation operation) {

		this.id = id;
		this.key = key;
		this.object = object;
		this.operation = operation;

		if (logger.isDebugEnabled()) {
			logger.debug("Instanciated Cache: ID" + this.id + " / " + key + " OP: " + this.operation
					+ " / " + object.toString());
		}
	}

	/**
	 * Set the new object version of this cache.
	 * 
	 * @param object
	 */
	public void setObject(Object object) {
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
		if (ObjectCache.logger.isDebugEnabled()) {
			ObjectCache.logger.debug("Updating Operation of ID " + this.id + " from " + this.operation + " to "
					+ newOperation);
		}
		this.operation = newOperation;
	}

	/**
	 * @return the id
	 */
	public long getId() {
		return this.id;
	}

	/**
	 * @return the key
	 */
	public String getKey() {
		return this.key;
	}
	
	/**
	 * @return the operation
	 */
	public Operation getOperation() {
		return this.operation;
	}

	/**
	 * @return the object
	 */
	public Object getObject() {
		return this.object;
	}
}
