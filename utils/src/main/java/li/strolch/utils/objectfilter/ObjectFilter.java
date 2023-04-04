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
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import li.strolch.utils.collections.MapOfMaps;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class implements a filter where modifications to an object are collected, and only the most recent action and
 * version of the object is returned.
 * <p>
 * In its current implementation, any instance of an object may "live" in the cache registered only under one single
 * key.
 * </p>
 * <p>
 * The rules of the cache are defined as follows. The top row represents the state of the cache for an object, given in
 * version O1. Here, "N/A" symbolizes that the object is not yet present in the cache or, if it is a result of an
 * operation, that it is removed from the cache. Each other row symbolizes the next action that is performed on an
 * object, with the cell containing the "final" action for this object with the version of the object to be retained.
 * Err! symbolize incorrect sequences of events that cause an exception.
 * </p>
 *
 * <table border="1" summary="Action state matrix">
 *
 * <tr>
 * <td>Action \ State in Cache</td>
 * <td>N/A</td>
 * <td>Add(01)</td>
 * <td>Update(O1)</td>
 * <td>Remove(O1)</td>
 * </tr>
 *
 * <tr>
 * <td>Add (O2)</td>
 * <td>Add(O2)</td>
 * <td>Err!</td>
 * <td>Err!</td>
 * <td>Update(O2)</td>
 * </tr>
 *
 * <tr>
 * <td>Update (O2)</td>
 * <td>Update(O2)</td>
 * <td>Add(O2)</td>
 * <td>Update(O2)</td>
 * <td>Err!</td>
 * </tr>
 *
 * <tr>
 * <td>Remove (O2)</td>
 * <td>Remove(O2)</td>
 * <td>N/A</td>
 * <td>Remove(O2)</td>
 * <td>Err!</td>
 * </tr>
 *
 * </table>
 *
 * @author Michael Gatto &lt;michael@gatto.ch&gt; (initial version)
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt; (minor modifications, refactorings)
 */
public class ObjectFilter {

	private final static Logger logger = LoggerFactory.getLogger(ObjectFilter.class);

	private static long id = ObjectCache.UNSET;

	private final MapOfMaps<String, Object, ObjectCache> cache;
	private final Set<String> keySet;

	/**
	 * Default constructor initializing the filter
	 */
	public ObjectFilter() {
		this.cache = new MapOfMaps<>();
		this.keySet = new HashSet<>();
	}

	private void replaceKey(ObjectCache cached, Object newObjectKey, Object newObject) {
		if (cached.getObjectKey() != newObjectKey) {
			if (ObjectFilter.logger.isDebugEnabled()) {
				String msg = "Replacing key for object as they are not the same reference: old: {0} / new: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, cached.getObjectKey(), newObjectKey);
				ObjectFilter.logger.warn(msg);
			}
			ObjectCache objectCache = this.cache.removeElement(cached.getKey(), cached.getObjectKey());
			this.cache.addElement(objectCache.getKey(), newObjectKey, objectCache);
		}
	}

	/**
	 * Register, under the given key, the addition of the given object.
	 * <p>
	 * This is the single point where the updating logic is applied for the cache in case of addition. The logic is:
	 * <table border="1" summary="Allowed operations">
	 * <tr>
	 * <td>Action\State in Cache</td>
	 * <td>N/A</td>
	 * <td>Add(01)</td>
	 * <td>Update(O1)</td>
	 * <td>Remove(O1)</td>
	 * </tr>
	 * <tr>
	 * <td>Add (O2)</td>
	 * <td>Add(O2)</td>
	 * <td>Err!</td>
	 * <td>Err!</td>
	 * <td>Update(O2)</td>
	 * </tr>
	 * </table>
	 *
	 * @param key
	 * 		the key to register the object with
	 * @param objectKey
	 * 		the key for the object
	 * @param objectToAdd
	 * 		The object for which addition shall be registered.
	 */
	public void add(String key, Object objectKey, Object objectToAdd) {

		if (ObjectFilter.logger.isDebugEnabled())
			ObjectFilter.logger
					.debug(MessageFormat.format("add object {0} with key {1}", objectToAdd, key)); //$NON-NLS-1$

		// BEWARE: you fix a bug here, be sure to update BOTH tables on the logic.
		ObjectCache cached = this.cache.getElement(key, objectKey);
		if (cached == null) {

			// The object has not yet been added to the cache.
			// Hence, we add it now, with the ADD operation.
			ObjectCache cacheObj = new ObjectCache(dispenseID(), key, objectKey, objectToAdd, Operation.ADD);
			this.cache.addElement(key, objectKey, cacheObj);

		} else {

			String existingKey = cached.getKey();
			if (!existingKey.equals(key)) {
				String msg = "Invalid key provided for object with transaction ID {0} and operation {1}:  existing key is {2}, new key is {3}. Object may be present in the same filter instance only once, registered using one key only. Object:{4}"; //$NON-NLS-1$
				throw new IllegalArgumentException(MessageFormat
						.format(msg, Long.toString(id), Operation.ADD.toString(), existingKey, key,
								objectKey.toString()));
			}

			// The object is in cache: update the version as required, keeping in mind that most
			// of the cases here will be mistakes...
			Operation op = cached.getOperation();
			switch (op) {
			case ADD -> throw new IllegalStateException(
					"Stale State exception: Invalid + after + for " + objectKey); //$NON-NLS-1$
			case MODIFY -> throw new IllegalStateException(
					"Stale State exception: Invalid + after += for " + objectKey); //$NON-NLS-1$
			case REMOVE -> {
				// replace key if necessary
				replaceKey(cached, objectKey, objectToAdd);

				// update operation's object
				cached.setObject(objectToAdd);
				cached.setOperation(Operation.MODIFY);
			}
			default -> throw new IllegalStateException("Stale State exception: Unhandled state " + op); //$NON-NLS-1$
			} // switch
		} // else of object not in cache

		// register the key
		this.keySet.add(key);
	}

	/**
	 * Register, under the given key, the update of the given object.
	 *
	 * <table border="1" summary="Allowed operations">
	 * <tr>
	 * <td>Action \ State in Cache</td>
	 * <td>N/A</td>
	 * <td>Add(01)</td>
	 * <td>Update(O1)</td>
	 * <td>Remove(O1)</td>
	 * </tr>
	 * <tr>
	 * <td>Update (O2)</td>
	 * <td>Update(O2)</td>
	 * <td>Add(O2)</td>
	 * <td>Update(O2)</td>
	 * <td>Err!</td>
	 * </tr>
	 * </table>
	 *
	 * @param key
	 * 		the key to register the object with
	 * @param objectKey
	 * 		the key for the object
	 * @param objectToUpdate
	 * 		The object for which update shall be registered.
	 */
	public void update(String key, Object objectKey, Object objectToUpdate) {

		if (ObjectFilter.logger.isDebugEnabled())
			ObjectFilter.logger
					.debug(MessageFormat.format("update object {0} with key {1}", objectKey, key)); //$NON-NLS-1$

		// BEWARE: you fix a bug here, be sure to update BOTH tables on the logic.
		ObjectCache cached = this.cache.getElement(key, objectKey);

		if (cached == null) {

			// The object got an ID during this run, but was not added to this cache.
			// Hence, we add it now, with the current operation.
			ObjectCache cacheObj = new ObjectCache(dispenseID(), key, objectKey, objectToUpdate, Operation.MODIFY);
			this.cache.addElement(key, objectKey, cacheObj);

		} else {

			// The object is in cache: update the version as required.
			Operation op = cached.getOperation();
			switch (op) {
			case ADD, MODIFY -> {
				// replace key if necessary
				replaceKey(cached, objectKey, objectToUpdate);

				// update operation's object
				cached.setObject(objectToUpdate);
			}
			case REMOVE -> throw new IllegalStateException(
					"Stale State exception: Invalid += after - for " + objectKey); //$NON-NLS-1$
			default -> throw new IllegalStateException("Stale State exception: Unhandled state " + op); //$NON-NLS-1$
			} // switch
		} // else of object not in cache

		// register the key
		this.keySet.add(key);
	}

	/**
	 * Register, under the given key, the removal of the given object.
	 *
	 * <table border="1" summary="allowed operations">
	 * <tr>
	 * <td>Action \ State in Cache</td>
	 * <td>N/A</td>
	 * <td>Add(01)</td>
	 * <td>Update(O1)</td>
	 * <td>Remove(O1)</td>
	 * </tr>
	 * <tr>
	 * <td>Remove (O2)</td>
	 * <td>Remove(O2)</td>
	 * <td>N/A</td>
	 * <td>Remove(O2)</td>
	 * <td>Err!</td>
	 * </tr>
	 * </table>
	 *
	 * @param key
	 * 		the key to register the object with
	 * @param objectKey
	 * 		the key for the object
	 * @param objectToRemove
	 * 		The object for which removal shall be registered.
	 */
	public void remove(String key, Object objectKey, Object objectToRemove) {

		if (ObjectFilter.logger.isDebugEnabled())
			ObjectFilter.logger
					.debug(MessageFormat.format("remove object {0} with key {1}", objectKey, key)); //$NON-NLS-1$

		// BEWARE: you fix a bug here, be sure to update BOTH tables on the logic.
		ObjectCache cached = this.cache.getElement(key, objectKey);
		if (cached == null) {
			// The object got an ID during this run, but was not added to this cache.
			// Hence, we add it now, with the current operation.
			ObjectCache cacheObj = new ObjectCache(dispenseID(), key, objectKey, objectToRemove, Operation.REMOVE);
			this.cache.addElement(key, objectKey, cacheObj);
		} else {

			String existingKey = cached.getKey();
			if (!existingKey.equals(key)) {
				String msg = "Invalid key provided for object with transaction ID {0} and operation {1}:  existing key is {2}, new key is {3}. Object may be present in the same filter instance only once, registered using one key only. Object:{4}"; //$NON-NLS-1$
				throw new IllegalArgumentException(MessageFormat
						.format(msg, Long.toString(id), Operation.REMOVE.toString(), existingKey, key,
								objectKey.toString()));
			}

			// The object is in cache: update the version as required.
			Operation op = cached.getOperation();
			switch (op) {
			case ADD ->
				// this is a case where we're removing the object from the cache, since we are
				// removing it now and it was added previously.
					this.cache.removeElement(key, objectKey);
			case MODIFY -> {
				// replace key if necessary
				replaceKey(cached, objectKey, objectToRemove);

				// update operation's object
				cached.setObject(objectToRemove);
				cached.setOperation(Operation.REMOVE);
			}
			case REMOVE -> throw new IllegalStateException(
					"Stale State exception: Invalid - after - for " + objectKey); //$NON-NLS-1$
			default -> throw new IllegalStateException("Stale State exception: Unhandled state " + op); //$NON-NLS-1$
			} // switch
		}

		// register the key
		this.keySet.add(key);
	}

	/**
	 * Register the addition of the given object. Since no key is provided, the class name is used as a key.
	 *
	 * @param object
	 * 		The object that shall be registered for addition
	 */
	public void add(Object object) {
		add(object.getClass().getName(), object, object);
	}

	/**
	 * Register the addition of the given object. Since no key is provided, the class name is used as a key.
	 *
	 * @param objectKey
	 * 		the key for the object
	 * @param object
	 * 		The object that shall be registered for addition
	 */
	public void add(Object objectKey, Object object) {
		add(object.getClass().getName(), objectKey, object);
	}

	/**
	 * Register the update of the given object. Since no key is provided, the class name is used as a key.
	 *
	 * @param object
	 * 		The object that shall be registered for updating
	 */
	public void update(Object object) {
		update(object.getClass().getName(), object, object);
	}

	/**
	 * Register the update of the given object. Since no key is provided, the class name is used as a key.
	 *
	 * @param objectKey
	 * 		the key for the object
	 * @param object
	 * 		The object that shall be registered for updating
	 */
	public void update(Object objectKey, Object object) {
		update(object.getClass().getName(), objectKey, object);
	}

	/**
	 * Register the removal of the given object. Since no key is provided, the class name is used as a key.
	 *
	 * @param object
	 * 		The object that shall be registered for removal
	 */
	public void remove(Object object) {
		remove(object.getClass().getName(), object, object);
	}

	/**
	 * Register the removal of the given object. Since no key is provided, the class name is used as a key.
	 *
	 * @param objectKey
	 * 		the key for the object
	 * @param object
	 * 		The object that shall be registered for removal
	 */
	public void remove(Object objectKey, Object object) {
		remove(object.getClass().getName(), objectKey, object);
	}

	/**
	 * Returns the ObjectCache for the given key and objectKey
	 *
	 * @param key
	 * 		the key under which it was registered
	 * @param objectKey
	 * 		the objectKey
	 *
	 * @return the ObjectCache, or null
	 */
	public ObjectCache getCache(String key, Object objectKey) {
		return this.cache.getElement(key, objectKey);
	}

	/**
	 * Returns the element with the given key and objectKey
	 *
	 * @param key
	 * 		the key under which it was registered
	 * @param objectKey
	 * 		the objectKey
	 * @param <V>
	 * 		the class to which to cast the element to
	 *
	 * @return the element, or null
	 */
	public <V> V getElement(String key, Object objectKey) {
		ObjectCache cache = this.cache.getElement(key, objectKey);
		if (cache == null)
			return null;
		@SuppressWarnings("unchecked")
		V element = (V) cache.getObject();
		return element;
	}

	/**
	 * Return the {@link Operation} for the given keys
	 *
	 * @param key
	 * 		the key under which it was registered
	 * @param objectKey
	 * 		the objectKey
	 *
	 * @return the {@link Operation} for the given keys
	 */
	public Operation getOperation(String key, Object objectKey) {
		ObjectCache objectCache = this.cache.getElement(key, objectKey);
		if (objectCache == null)
			return null;
		return objectCache.getOperation();
	}

	/**
	 * Return true if the element with the given key and objectKey exist
	 *
	 * @param key
	 * 		the key under which it was registered
	 * @param objectKey
	 * 		the objectKey
	 *
	 * @return true if the element with the given key and objectKey exist
	 */
	public boolean hasElement(String key, Object objectKey) {
		return this.cache.containsElement(key, objectKey);
	}

	/**
	 * Allows clearing the object cache for an element
	 *
	 * @param key
	 * 		the key under which it was registered
	 * @param objectKey
	 * 		the objectKey
	 */
	public void removeObjectCache(String key, Object objectKey) {
		this.keySet.remove(key);
		this.cache.removeElement(key, objectKey);
	}

	/**
	 * Streams all objects with the given operation and key
	 *
	 * @param operation
	 * 		the operation to match
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public Stream<Object> streamFor(String key, Operation operation) {
		if (!this.cache.containsMap(key))
			return Stream.empty();
		return this.cache.getAllElements(key).stream() //
				.filter(objectCache -> objectCache.getOperation() == operation) //
				.map(ObjectCache::getObject);
	}

	/**
	 * Streams all objects with the given key
	 *
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public Stream<Object> streamFor(String key) {
		if (!this.cache.containsMap(key))
			return Stream.empty();
		return this.cache.getAllElements(key).stream() //
				.map(ObjectCache::getObject);
	}

	/**
	 * Streams all objects with the given key
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public <V> Stream<V> streamFor(Class<V> clazz) {
		return this.cache.stream().flatMap(m -> m.getValue().values().stream()) //
				.filter(objectCache -> objectCache.getObject().getClass() == clazz) //
				.map(objectCache -> {
					@SuppressWarnings("unchecked")
					V v = (V) objectCache.getObject();
					return v;
				});
	}

	/**
	 * Streams all objects with the given key
	 *
	 * @param clazz
	 * 		the class to match
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public <V> Stream<V> streamFor(Class<V> clazz, String key) {
		if (!this.cache.containsMap(key))
			return Stream.empty();
		return this.cache.getAllElements(key).stream() //
				.filter(objectCache -> objectCache.getObject().getClass() == clazz) //
				.map(objectCache -> {
					@SuppressWarnings("unchecked")
					V v = (V) objectCache.getObject();
					return v;
				});
	}

	/**
	 * Streams all objects with the given operation, key and clazz type
	 *
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public <V> Stream<V> streamFor(Class<V> clazz, String key, Operation operation) {
		return streamFor(key, operation).filter(o -> o.getClass() == clazz) //
				.map(o -> {
					@SuppressWarnings("unchecked")
					V v = (V) o;
					return v;
				});
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an addition.
	 *
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public Stream<Object> streamAdded(String key) {
		return streamFor(key, Operation.ADD);
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an addition.
	 *
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public List<Object> getAdded(String key) {
		return streamAdded(key).collect(Collectors.toList());
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an addition.
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public <V> Stream<V> streamAdded(Class<V> clazz, String key) {
		return streamFor(clazz, key, Operation.ADD);
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an addition.
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be added.
	 */
	public <V> List<V> getAdded(Class<V> clazz, String key) {
		return streamAdded(clazz, key).collect(Collectors.toList());
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an update.
	 *
	 * @param key
	 * 		registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be updated.
	 */
	public Stream<Object> streamUpdated(String key) {
		return streamFor(key, Operation.MODIFY);
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an update.
	 *
	 * @param key
	 * 		registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be updated.
	 */
	public List<Object> getUpdated(String key) {
		return streamUpdated(key).collect(Collectors.toList());
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an update.
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be updated.
	 */
	public <V> Stream<V> streamUpdated(Class<V> clazz, String key) {
		return streamFor(clazz, key, Operation.MODIFY);
	}

	/**
	 * Get all objects that were registered under the given key and that have as a resulting final action an update.
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		registration key of the objects to match
	 *
	 * @return The list of all objects registered under the given key and that need to be updated.
	 */
	public <V> List<V> getUpdated(Class<V> clazz, String key) {
		return streamUpdated(clazz, key).collect(Collectors.toList());
	}

	/**
	 * Get all objects that were registered under the given key that have as a resulting final action their removal.
	 *
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of object registered under the given key that have, as a final action, removal.
	 */
	public Stream<Object> streamRemoved(String key) {
		return streamFor(key, Operation.REMOVE);
	}

	/**
	 * Get all objects that were registered under the given key that have as a resulting final action their removal.
	 *
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of object registered under the given key that have, as a final action, removal.
	 */
	public List<Object> getRemoved(String key) {
		return streamRemoved(key).collect(Collectors.toList());
	}

	/**
	 * Get all objects that were registered under the given key that have as a resulting final action their removal.
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of object registered under the given key that have, as a final action, removal.
	 */
	public <V> Stream<V> streamRemoved(Class<V> clazz, String key) {
		return streamFor(clazz, key, Operation.REMOVE);
	}

	/**
	 * Get all objects that were registered under the given key that have as a resulting final action their removal.
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of object registered under the given key that have, as a final action, removal.
	 */
	public <V> List<V> getRemoved(Class<V> clazz, String key) {
		return streamRemoved(clazz, key).collect(Collectors.toList());
	}

	/**
	 * Get all objects that were registered under the given key
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of object registered under the given key that have, as a final action, removal.
	 */
	public <V> Stream<V> streamAll(Class<V> clazz, String key) {
		return streamFor(clazz, key);
	}

	/**
	 * Get all objects that were registered under the given key
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 * @param key
	 * 		The registration key of the objects to match
	 *
	 * @return The list of object registered under the given key that have, as a final action, removal.
	 */
	public <V> List<V> getAll(Class<V> clazz, String key) {
		return streamAll(clazz, key).collect(Collectors.toList());
	}

	/**
	 * Get all objects that of the given class
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 *
	 * @return The list of all objects that of the given class
	 */
	public <V> Stream<V> streamAll(Class<V> clazz) {
		return streamFor(clazz);
	}

	/**
	 * Get all objects that of the given class
	 *
	 * @param clazz
	 * 		The class type of the object to be retrieved, that acts as an additional filter criterion.
	 *
	 * @return The list of all objects that of the given class
	 */
	public <V> List<V> getAll(Class<V> clazz) {
		return streamAll(clazz).collect(Collectors.toList());
	}

	/**
	 * Get all the objects that were processed in this transaction, that were registered under the given key. No action
	 * is associated to the object.
	 *
	 * @param key
	 * 		The registration key for which the objects shall be retrieved
	 *
	 * @return The list of objects matching the given key.
	 */
	public Stream<Object> streamAll(String key) {
		return streamFor(key);
	}

	/**
	 * Get all the objects that were processed in this transaction, that were registered under the given key. No action
	 * is associated to the object.
	 *
	 * @param key
	 * 		The registration key for which the objects shall be retrieved
	 *
	 * @return The list of objects matching the given key.
	 */
	public List<Object> getAll(String key) {
		return streamAll(key).collect(Collectors.toList());
	}

	/**
	 * Get all the objects that were processed in this transaction, that were registered under the given key. No action
	 * is associated to the object.
	 *
	 * @param key
	 * 		The registration key for which the objects shall be retrieved
	 *
	 * @return The list of objects matching the given key.
	 */
	public List<ObjectCache> getCache(String key) {
		if (!this.cache.containsMap(key))
			return Collections.emptyList();
		return this.cache.getAllElements(key);
	}

	/**
	 * Return the set of keys that are currently present in the object filter.
	 *
	 * @return The set containing the keys of that have been added to the filter.
	 */
	public Set<String> keySet() {
		return this.keySet;
	}

	/**
	 * Clears the cache
	 */
	public void clearCache() {
		this.cache.clear();
		this.keySet.clear();
	}

	/**
	 * @return the set of keys used to register objects
	 */
	public int sizeKeySet() {
		return this.keySet.size();
	}

	/**
	 * @return the number of objects registered in this filter
	 */
	public int sizeCache() {
		return this.cache.size();
	}

	/**
	 * @return true if the cache is empty, false otherwise
	 */
	public boolean isEmpty() {
		return this.cache.size() == 0;
	}

	/**
	 * @return get a unique transaction ID
	 */
	public synchronized long dispenseID() {
		ObjectFilter.id++;
		if (ObjectFilter.id == Long.MAX_VALUE) {
			ObjectFilter.logger.error("Rolling IDs of objectFilter back to 1. Hope this is fine."); //$NON-NLS-1$
			ObjectFilter.id = 1;
		}
		return ObjectFilter.id;
	}
}
