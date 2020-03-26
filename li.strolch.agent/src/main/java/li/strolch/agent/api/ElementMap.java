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
package li.strolch.agent.api;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

/**
 * <p>
 * The {@link ElementMap} is the entry point to the Strolch model. Any access to Strolch objects is done using the
 * {@link ElementMap}. A concrete {@link ElementMap} instance uses the given {@link StrolchTransaction} to access the a
 * {@link StrolchDao} and perform the relevant operation.
 * </p>
 *
 * <p>
 * <b>Note:</b>
 * </p>
 *
 * @param <T>
 * 		the object instance being managed
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ElementMap<T extends StrolchRootElement> {

	/**
	 * Returns true if the underlying persistence layer has elements with the given type
	 *
	 * @param tx
	 * 		the open {@link StrolchTransaction}
	 * @param type
	 * 		the type of element to check for
	 *
	 * @return true if the underlying persistence layer has elements with the given type
	 */
	boolean hasType(StrolchTransaction tx, String type);

	/**
	 * Returns true if the underlying persistence layer has an element with the given type and ID
	 *
	 * @param tx
	 * 		the open {@link StrolchTransaction}
	 * @param type
	 * 		the type of element to check for
	 * @param id
	 * 		the ID of the element to check for
	 *
	 * @return true if the underlying persistence layer has an element with the given type and ID
	 */
	boolean hasElement(StrolchTransaction tx, String type, String id);

	/**
	 * Returns the number of elements regardless of type in the underlying persistence layer
	 *
	 * @param tx
	 * 		the open {@link StrolchTransaction}
	 *
	 * @return the number of elements regardless of type in the underlying persistence layer
	 */
	long querySize(StrolchTransaction tx);

	/**
	 * Returns the number of elements of the given type in the underlying persistence layer
	 *
	 * @param tx
	 * 		the open {@link StrolchTransaction}
	 * @param type
	 * 		the type of element for which the number of elements is to be queried
	 *
	 * @return the number of elements of the given type in the underlying persistence layer
	 */
	long querySize(StrolchTransaction tx, String type);

	/**
	 * Returns a copy of the element with the type "Template" and the id = type
	 *
	 * @param tx
	 * 		the open {@link StrolchTransaction}
	 * @param type
	 * 		The template id to return
	 *
	 * @return the template, or null if it does not exist
	 */
	T getTemplate(StrolchTransaction tx, String type);

	/**
	 * Returns a copy of the element with the type "Template" and the id = type
	 *
	 * @param tx
	 * 		the open {@link StrolchTransaction}
	 * @param type
	 * 		The template id to return
	 * @param assertExists
	 * 		if true, and element does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the template, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the template does not exist
	 */
	T getTemplate(StrolchTransaction tx, String type, boolean assertExists) throws StrolchException;

	/**
	 * Retrieves the element with the given type and id, or null if it does not exist
	 *
	 * @param tx
	 * 		the open transaction
	 * @param type
	 * 		the type of the element to retrieve
	 * @param id
	 * 		the id of the element to retrieve
	 *
	 * @return the element with the type and id, or null if it does not exist
	 */
	T getBy(StrolchTransaction tx, String type, String id);

	/**
	 * Retrieves the element with the given type and id, or null if it does not exist
	 *
	 * @param tx
	 * 		the open transaction
	 * @param type
	 * 		the type of the element to retrieve
	 * @param id
	 * 		the id of the element to retrieve
	 * @param assertExists
	 * 		if true, and element does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the element with the type and id, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the element does not exist
	 */
	T getBy(StrolchTransaction tx, String type, String id, boolean assertExists) throws StrolchException;

	/**
	 * Retrieves the specific version of the element with the given type and id, or null if it does not exist
	 *
	 * @param tx
	 * 		the open transaction
	 * @param type
	 * 		the type of the element to retrieve
	 * @param id
	 * 		the id of the element to retrieve
	 * @param version
	 * 		the version to get
	 *
	 * @return the element with the type and id, or null if it does not exist
	 */
	T getBy(StrolchTransaction tx, String type, String id, int version);

	/**
	 * Retrieves the specific version of the element with the given type and id, or null if it does not exist
	 *
	 * @param tx
	 * 		the open transaction
	 * @param type
	 * 		the type of the element to retrieve
	 * @param id
	 * 		the id of the element to retrieve
	 * @param version
	 * 		the version to get
	 * @param assertExists
	 * 		if true, and element does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the element with the type and id, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the element does not exist
	 */
	T getBy(StrolchTransaction tx, String type, String id, int version, boolean assertExists)
			throws StrolchException;

	/**
	 * Returns the element which is referenced by the given {@link StringParameter}. A reference {@link Parameter} must
	 * have its interpretation set to the element type being referenced e.g. s
	 * {@link StrolchConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to the element's type and the value is
	 * the id of the element
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param refP
	 * 		the {@link StringParameter} which references an element
	 * @param assertExists
	 * 		if true, and element does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the element found, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	T getBy(StrolchTransaction tx, StringParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Returns all elements which are referenced by the given {@link StringListParameter}. A reference {@link Parameter}
	 * must have its interpretation set to the element type being referenced e.g. s
	 * {@link StrolchConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to the element's type and the value is
	 * the id of the element
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param refP
	 * 		the {@link StringListParameter} which references an element
	 * @param assertExists
	 * 		if true, and element does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the list of elements found, or the empty list if they do not exist. <b>Note:</b> Any missing elements are
	 * not returned!
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	List<T> getBy(StrolchTransaction tx, StringListParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Queries and returns all the versions of the element with the given type and ID
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param type
	 * 		the type of the element to be queried
	 * @param id
	 * 		the id of the element to be queried
	 *
	 * @return all the versions of the element with the given type and ID
	 */
	List<T> getVersionsFor(StrolchTransaction tx, String type, String id);

	/**
	 * Returns the latest version of the given element, or -1 if no version available
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param type
	 * 		the type of the element
	 * @param id
	 * 		the id of the element
	 *
	 * @return the latest version of the element, or -1 if no version available
	 */
	int getLatestVersionFor(StrolchTransaction tx, String type, String id);

	/**
	 * Returns all elements in the underlying persistence layer regardless of type
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 *
	 * @return all elements in the underlying persistence layer regardless of type
	 */
	List<T> getAllElements(StrolchTransaction tx);

	/**
	 * Returns all elements in the underlying persistence layer of the given type
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param type
	 * 		the type of the elements to retrieve
	 *
	 * @return all elements in the underlying persistence layer of the given type
	 */
	List<T> getElementsBy(StrolchTransaction tx, String type);

	/**
	 * Returns a {@link Stream} for all elements for the given types
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param types
	 * 		the types of the elements
	 *
	 * @return a stream for the elements
	 */
	Stream<T> stream(StrolchTransaction tx, String... types);

	/**
	 * Returns all the types known in the underlying persistence layer
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 *
	 * @return all the types known in the underlying persistence layer
	 */
	Set<String> getTypes(StrolchTransaction tx);

	/**
	 * Returns all keys/IDs of all elements in the underlying persistence layer, regardless of type
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 *
	 * @return all keys/IDs of all elements in the underlying persistence layer, regardless of type
	 */
	Set<String> getAllKeys(StrolchTransaction tx);

	/**
	 * Returns all keys/IDs of all elements in the underlying persistence layer, of the given type
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param type
	 * 		the type of the element to retrieve the keys for
	 *
	 * @return all keys/IDs of all elements in the underlying persistence layer, of the given type
	 */
	Set<String> getKeysBy(StrolchTransaction tx, String type);

	/**
	 * Adds the given element to the underlying persistence layer. The element may not already exist
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param element
	 * 		the element to add
	 *
	 * @throws StrolchPersistenceException
	 * 		if an element already exists with the same ID
	 */
	void add(StrolchTransaction tx, T element) throws StrolchPersistenceException;

	/**
	 * Adds the given elements to the underlying persistence layer. None of the elements may already exist
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param elements
	 * 		the elements to add
	 *
	 * @throws StrolchPersistenceException
	 * 		if an element already exists with the same ID
	 */
	void addAll(StrolchTransaction tx, List<T> elements) throws StrolchPersistenceException;

	/**
	 * Updates the existing element
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param element
	 * 		the element to update
	 *
	 * @return the replaced element
	 *
	 * @throws StrolchPersistenceException
	 * 		if the element does not exist
	 */
	void update(StrolchTransaction tx, T element) throws StrolchPersistenceException;

	/**
	 * Updates all the existing elements
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param elements
	 * 		the elements to update
	 *
	 * @return the replaced elements
	 *
	 * @throws StrolchPersistenceException
	 * 		if any of the elements don't yet exist
	 */
	void updateAll(StrolchTransaction tx, List<T> elements) throws StrolchPersistenceException;

	/**
	 * Removes the given element
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param element
	 * 		the element to be removed
	 *
	 * @throws StrolchPersistenceException
	 * 		if the element does not exist
	 */
	void remove(StrolchTransaction tx, T element) throws StrolchPersistenceException;

	/**
	 * Removes the given elements
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param elements
	 * 		the elements to be removed
	 *
	 * @throws StrolchPersistenceException
	 * 		if any of the elements don't yet exist
	 */
	void removeAll(StrolchTransaction tx, List<T> elements) throws StrolchPersistenceException;

	/**
	 * <p>
	 * Removes all elements regardless of the type
	 * </p>
	 *
	 * <p>
	 * <b>Note:</b> This method ignores versioning. Do NOT call this method unless you want to clear the model!
	 * </p>
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 *
	 * @return the number of elements removed
	 */
	long removeAll(StrolchTransaction tx);

	/**
	 * <p>
	 * Removes all elements of the given type
	 * </p>
	 *
	 * <p>
	 * <b>Note:</b> This method ignores versioning. Do NOT call this method unless you want to clear the model!
	 * </p>
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param type
	 * 		the type of elements to remove
	 *
	 * @return the number of elements removed
	 */
	long removeAllBy(StrolchTransaction tx, String type);

	/**
	 * <p>
	 * Undoes the given version by reverting to the previous version of the element given. If the element given is the
	 * first version, then the element is removed. If the previous version exists, then it is reverted to it. If the
	 * given element does not exist, nor it's previous version, then a {@link StrolchPersistenceException} is thrown
	 * </p>
	 *
	 * <p>
	 * <b>Note:</b> This method should only be used in the same transaction where the element was created to undo a
	 * change in the same transaction. If there is a requirement to revert to a previous version, then the
	 * {@link #revertToVersion(StrolchTransaction, StrolchRootElement)} method should be used.
	 * </p>
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param element
	 * 		the element which is to be reverted to a previous version
	 *
	 * @throws StrolchException
	 * 		if the version does not exist, if this version is not the latest version, if the previous version is
	 * 		missing or other problems arise
	 */
	void undoVersion(StrolchTransaction tx, T element) throws StrolchException;

	/**
	 * <p>
	 * Reverts to the given version of the specified element. This method will retrieve the given version of the
	 * specified element, then set a new version on that element which is an increment of the current latest version and
	 * store this new version. The new element is then returned for convenience.
	 * </p>
	 *
	 * <p>
	 * <b>Note:</b> This is the method which should be called when a change should be reverted. This method gurantees
	 * that the history is not changed and that a new version is saved but with the version of the element specified.
	 * </p>
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param element
	 * 		the version of the element to revert to
	 *
	 * @return the new version of the element
	 *
	 * @throws StrolchException
	 * 		if the version does not exist
	 */
	T revertToVersion(StrolchTransaction tx, T element) throws StrolchException;

	/**
	 * <p>
	 * Reverts to the given version of the specified element. This method will retrieve the given version of the
	 * specified element, then set a new version on that element which is an increment of the current latest version and
	 * store this new version. The new element is then returned for convenience.
	 * </p>
	 *
	 * <p>
	 * <b>Note:</b> This is the method which should be called when a change should be reverted. This method gurantees
	 * that the history is not changed and that a new version is saved but with the version of the element specified.
	 * </p>
	 *
	 * @param tx
	 * 		the {@link StrolchTransaction} instance
	 * @param type
	 * 		the type of the element to revert to
	 * @param id
	 * 		the id of the element to revert to
	 * @param version
	 * 		the version of the specified element to revert to
	 *
	 * @return the new version of the element
	 *
	 * @throws StrolchException
	 * 		if the version does not exist
	 */
	T revertToVersion(StrolchTransaction tx, String type, String id, int version) throws StrolchException;
}
