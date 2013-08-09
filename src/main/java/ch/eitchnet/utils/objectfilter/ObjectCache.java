/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.utils
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
