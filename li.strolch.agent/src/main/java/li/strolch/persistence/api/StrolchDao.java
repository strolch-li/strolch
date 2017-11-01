/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.persistence.api;

import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchRootElement;

/**
 * <p>
 * This data access object is used to access {@link StrolchRootElement} from the underlying persistence layer. Objects
 * in Strolch are always referenced by a type and an ID. The type is a categorisation/grouping of the objects, while the
 * ID is a unique identifier of the object. The ID must be unique, even for multiple groups.
 * </p>
 * 
 * <p>
 * The DAO must support versioning. i.e. if versioning is enabled, then if an object is updated or removed, the existing
 * version is not modified, but a new version is persisted so that rollbacks can be done
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 * @param <T>
 *            the object instance being queried from the underlying persistence layer
 */
public interface StrolchDao<T extends StrolchRootElement> {

	/**
	 * Queries the set of IDs for all elements, regardless of types from the underlying persistence layer.
	 * 
	 * @return the set of IDs for all elements
	 */
	public Set<String> queryKeySet();

	/**
	 * Returns true if an element exists with the given type and id in the underlying persistence layer
	 * 
	 * @param type
	 *            the type of elements
	 * @param id
	 *            the id of the element to return
	 * 
	 * @return true if an element exists with the given type and id
	 */
	public boolean hasElement(String type, String id);

	/**
	 * Returns the number of elements in the underlying persistence layer, regardless of type
	 * 
	 * @return the number of elements in the underlying persistence layer
	 */
	public long querySize();

	/**
	 * Returns the number of elements in the underlying persistence layer for the given type
	 * 
	 * @return the number of elements in the underlying persistence layer for the given type
	 */
	public long querySize(String type);

	/**
	 * Queries the set of IDs for the elements with the given type
	 * 
	 * @return the set of IDs for the elements with the given type
	 */
	public Set<String> queryKeySet(String type);

	/**
	 * Queries the current list of types from the underlying persistence layer
	 * 
	 * @return the list of types
	 */
	public Set<String> queryTypes();

	/**
	 * Queries the latest version of the element with the given type and ID
	 * 
	 * @param type
	 *            the type of the element to be queried
	 * @param id
	 *            the id of the element to be queried
	 * 
	 * @return the element with the given type and ID, or null if it does not exist
	 */
	public T queryBy(String type, String id);

	/**
	 * <p>
	 * Queries a specific version of the element with the given type and ID.
	 * </p>
	 * 
	 * <p>
	 * <b>Note:</b> If you want to query the latest version, then use the method with out the version parameter
	 * </p>
	 * 
	 * @param type
	 *            the type of the element to be queried
	 * @param id
	 *            the id of the element to be queried
	 * @param version
	 *            the version of the element to be returned
	 * 
	 * @return the element with the given type and ID, or null if it does not exist
	 */
	public T queryBy(String type, String id, int version);

	/**
	 * Queries and returns all the versions of the element with the given type and ID
	 * 
	 * @param type
	 *            the type of the element to be queried
	 * @param id
	 *            the id of the element to be queried
	 * 
	 * @return all the versions of the element with the given type and ID
	 */
	public List<T> queryVersionsFor(String type, String id);

	/**
	 * Queries and returns the latest version of the element with the given type and ID, -1 if no version available
	 *
	 * @param type
	 *            the type of the element to be queried
	 * @param id
	 *            the id of the element to be queried
	 *
	 * @return the latest version of the element with the given type and ID, -1 if no version available
	 */
	public int queryLatestVersionFor(String type, String id);

	/**
	 * Queries and returns the number of versions for the element with the given type and ID
	 * 
	 * @param type
	 *            the type of the element to be queried
	 * @param id
	 *            the id of the element to be queried
	 * 
	 * @return the number of versions for the element with the given type and ID
	 */
	public long queryVersionsSizeFor(String type, String id);

	/**
	 * Queries and returns all elements regardless of type
	 * 
	 * @return all elements regardless of type
	 */
	public List<T> queryAll();

	/**
	 * Queries and returns all elements of the given type
	 * 
	 * @param type
	 *            the type of element to return
	 * 
	 * @return all elements of the given type
	 */
	public List<T> queryAll(String type);

	/**
	 * Persists the given element. The element must not yet exist
	 * 
	 * @param element
	 *            the element to be persisted
	 * 
	 * @throws StrolchPersistenceException
	 *             if the element already exists
	 */
	public void save(T element) throws StrolchPersistenceException;

	/**
	 * Persists the given list of elements. None of the elements may already exists
	 * 
	 * @param elements
	 *            the list of elements to be persisted
	 * 
	 * @throws StrolchPersistenceException
	 *             if any of the elements already exist
	 */
	public void saveAll(List<T> elements) throws StrolchPersistenceException;

	/**
	 * Updates the given element. The element must already exist
	 * 
	 * @param element
	 *            the element to be updated
	 * 
	 * @throws StrolchPersistenceException
	 *             if the element does not exist
	 */
	public void update(T element) throws StrolchPersistenceException;

	/**
	 * Updates the given list of elements. Each element must already exist
	 * 
	 * @param elements
	 *            the elements to be updated
	 * 
	 * @throws StrolchPersistenceException
	 *             if any of the elements do not exist
	 */
	public void updateAll(List<T> elements) throws StrolchPersistenceException;

	/**
	 * <p>
	 * Removes the given element from the underlying persistence layer
	 * </p>
	 * 
	 * <p>
	 * <b>Note:</b> This method deletes the given object including its versions! Do not call this method if you want to
	 * use versioning!
	 * </p>
	 * 
	 * @param element
	 *            the element to be removed
	 * 
	 * @throws StrolchPersistenceException
	 *             if the element does not exist
	 */
	public void remove(T element) throws StrolchPersistenceException;

	/**
	 * <p>
	 * Removes the given elements from the underlying persistence layer
	 * </p>
	 * 
	 * <p>
	 * <b>Note:</b> This method deletes the given objects including their versions! Do not call this method if you want
	 * to use versioning!
	 * </p>
	 * 
	 * 
	 * @param elements
	 *            the elements to be removed
	 * 
	 * @throws StrolchPersistenceException
	 *             if any of the elements do not exist
	 */
	public void removeAll(List<T> elements) throws StrolchPersistenceException;

	/**
	 * <p>
	 * Removes all elements regardless of type from the underlying persistence layer
	 * </p>
	 * 
	 * <p>
	 * <b>Note:</b> This method does not support versioning. This method completely removes all objects regardless of
	 * type and their versions!
	 * </p>
	 * 
	 * @return the number of elements removed
	 * 
	 * @throws StrolchPersistenceException
	 */
	public long removeAll();

	/**
	 * <p>
	 * Removes all elements of the given type from the underlying persistence layer
	 * </p>
	 * 
	 * <p>
	 * <b>Note:</b> This method does not support versioning. This method completely removes all objects of the given
	 * type and their versions!
	 * </p>
	 * 
	 * @param type
	 *            the type of element to remove
	 * 
	 * @return the number of elements removed
	 */
	public long removeAllBy(String type);

	/**
	 * <p>
	 * Removes the given version of the given element from the underlying persistence layer. The version must be the
	 * latest version and thus always deletes the newest version. To delete multiple versions call this method multiple
	 * times. To remove it completely, call the {@link #remove(StrolchRootElement)} method.
	 * </p>
	 * 
	 * <p>
	 * <b>Note:</b> This element given must be the current latest version!
	 * </p>
	 * 
	 * @param element
	 *            the latest version of the element to be removed
	 * 
	 * @throws StrolchPersistenceException
	 *             if the element/version does not exist
	 */
	public void removeVersion(T element) throws StrolchPersistenceException;

	/**
	 * Causes the DAO to flush any actions which have not yet been sent to the underlying persistence layer
	 */
	public void flush();
}
