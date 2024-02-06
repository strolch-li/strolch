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
import java.util.Locale;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import li.strolch.agent.api.*;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.policy.StrolchPolicy;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.api.Command;
import li.strolch.utils.objectfilter.ObjectFilterStatistics;

/**
 * <p>
 * {@link StrolchTransaction} is the central element in Strolch. It gives the developer access to the Strolch model and
 * performs all the required actions to keep the model consistent etc.
 * </p>
 *
 * <p>
 * A Strolch transaction is performed as follows as it is an {@link AutoCloseable} implementation
 * </p>
 *
 * <pre>
 * StrolchAgent strolchAgent = getStrolchAgent();
 * StrolchRealm realm = strolchAgent.getContainer().getRealm(StrolchConstants.DEFAULT_REALM);
 * try (StrolchTransaction tx = realm.openTx(certificate, getClass())) {
 *   // do work e.g. add commands
 *   tx.commitOnClose();
 * }
 * </pre>
 *
 * <p>
 * A {@link StrolchTransaction} is always opened for a specific realm, should no specific realms be configured, then the
 * {@link StrolchConstants#DEFAULT_REALM} is automatically created.
 * </p>
 *
 * <p>
 * A {@link StrolchTransaction} takes care of the following:
 * </p>
 *
 * <ul>
 * <li>Opening and closing database connections</li>
 * <li>Releasing locks to strolch elements, if {@link #lock(StrolchRootElement)} or {@link #lock(Locator)} is used</li>
 * <li>Performing Commands correctly</li>
 * <li>exception handling</li>
 * <li>auditing</li>
 * <li>updating observers</li>
 * </ul>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 * @see AbstractTransaction
 */
public interface StrolchTransaction extends AutoCloseable {

	/**
	 * Returns the action for which this transaction was opened
	 *
	 * @return the action for which this transaction was opened
	 */
	String getAction();

	/**
	 * Returns the name of the {@link StrolchRealm} for which this transaction was opened
	 *
	 * @return the name of the {@link StrolchRealm} for which this transaction was opened
	 */
	String getRealmName();

	/**
	 * Returns the {@link Certificate} which allowed this TX to be opened
	 *
	 * @return the {@link Certificate} which allowed this TX to be opened
	 */
	Certificate getCertificate();

	/**
	 * Returns the username of the {@link Certificate} currently executing this TX
	 *
	 * @return the username of the {@link Certificate} currently executing this TX
	 */
	default String getUsername() {
		return getCertificate().getUsername();
	}

	/**
	 * Returns the {@link Locale} for the {@link Certificate}
	 *
	 * @return the {@link Locale} for the {@link Certificate}
	 */
	Locale getLocale();

	/**
	 * Returns the {@link PrivilegeContext} for this TX's certificate
	 *
	 * @return the {@link PrivilegeContext} for this TX's certificate
	 */
	PrivilegeContext getPrivilegeContext();

	/**
	 * Validates the given {@link Restrictable} on the {@link PrivilegeContext}
	 *
	 * @param restrictable
	 * 		the restrictable to validate
	 *
	 * @throws PrivilegeException
	 * 		if the {@link PrivilegeContext} is invalid
	 * @throws AccessDeniedException
	 * 		if the user may not perform this action
	 */
	void validateAction(Restrictable restrictable) throws PrivilegeException, AccessDeniedException;

	/**
	 * Returns a reference to the {@link AuditTrail} for the {@link StrolchRealm} for which this transaction was opened
	 *
	 * @return the {@link AuditTrail}
	 */
	AuditTrail getAuditTrail();

	/**
	 * Returns a reference to the {@link ResourceMap} for the {@link StrolchRealm} for which this transaction was
	 * opened
	 *
	 * @return the {@link ResourceMap}
	 */
	ResourceMap getResourceMap();

	/**
	 * Returns a reference to the {@link OrderMap} for the {@link StrolchRealm} for which this transaction was opened
	 *
	 * @return the {@link OrderMap}
	 */
	OrderMap getOrderMap();

	/**
	 * Returns a reference to the {@link ActivityMap} for the {@link StrolchRealm} for which this transaction was
	 * opened
	 *
	 * @return the {@link ActivityMap}
	 */
	ActivityMap getActivityMap();

	/**
	 * Returns the count of all resources
	 *
	 * @return the count of all resources
	 */
	long getResourceCount();

	/**
	 * Returns the count of resources by the given type
	 *
	 * @return the count of resources by the given type
	 */
	long getResourceCount(String type);

	/**
	 * Returns the count of all orders
	 *
	 * @return the count of all orders
	 */
	long getOrderCount();

	/**
	 * Returns the count of orders by the given type
	 *
	 * @return the count of orders by the given type
	 */
	long getOrderCount(String type);

	/**
	 * Returns the count of all activities
	 *
	 * @return the count of all activities
	 */
	long getActivityCount();

	/**
	 * Returns the count of activities by the given type
	 *
	 * @return the count of activities by the given type
	 */
	long getActivityCount(String type);

	/**
	 * Returns the {@link PersistenceHandler}. If the {@link StrolchRealm} is not running in
	 * {@link DataStoreMode#TRANSIENT} mode, then the {@link PersistenceHandler} will be a {@link StrolchComponent},
	 * otherwise it will be the internal in memory persistence handler
	 *
	 * @return the {@link PersistenceHandler}
	 */
	PersistenceHandler getPersistenceHandler();

	/**
	 * Return the {@link StrolchAgent}
	 *
	 * @return the reference to the agent
	 */
	StrolchAgent getAgent();

	/**
	 * Return the {@link ComponentContainer}
	 *
	 * @return the reference to the container
	 */
	ComponentContainer getContainer();

	/**
	 * Instantiates the policy using the given {@link Class} to retrieve the {@link PolicyDef} from the given element
	 *
	 * @param element
	 * 		the element from which to retrieve the {@link PolicyDef
	 * @param <T>
	 * 		the type of policy to return
	 * @param clazz
	 * 		the type of policy to return
	 *
	 * @return the policy
	 */
	<T extends StrolchPolicy> T getPolicy(PolicyContainer element, Class<T> clazz);

	/**
	 * Instantiates the policy using the given {@link PolicyDef}
	 *
	 * @param policyDef
	 * 		the policy definition
	 * @param <T>
	 * 		the type of policy to return
	 *
	 * @return the policy
	 *
	 * @deprecated use {@link #getPolicy(Class, PolicyDef)} instead
	 */
	@Deprecated
	<T extends StrolchPolicy> T getPolicy(PolicyDef policyDef);

	/**
	 * Instantiates the policy using the given {@link PolicyDef}
	 *
	 * @param clazz
	 * 		the type of policy to return
	 * @param policyDef
	 * 		the policy definition
	 * @param <T>
	 * 		the type of policy to return
	 *
	 * @return the policy
	 */
	<T extends StrolchPolicy> T getPolicy(Class<T> clazz, PolicyDef policyDef);

	/**
	 * Instantiates the policy using the given {@link PolicyDef}, or if not available, the default policy
	 *
	 * @param policyDef
	 * 		the policy definition
	 * @param defaultDef
	 * 		the default policy definition if the given policy definition is unavailable
	 * @param <T>
	 * 		the type of policy to return
	 *
	 * @return the policy
	 *
	 * @deprecated user {@link #getPolicy(Class, PolicyDef, PolicyDef)} instead
	 */
	@Deprecated
	<T extends StrolchPolicy> T getPolicy(PolicyDef policyDef, PolicyDef defaultDef);

	/**
	 * Instantiates the policy using the given {@link PolicyDef}, or if not available, the default policy
	 *
	 * @param clazz
	 * 		the type of policy to return
	 * @param policyDef
	 * 		the policy definition
	 * @param defaultDef
	 * 		the default policy definition if the given policy definition is unavailable
	 * @param <T>
	 * 		the type of policy to return
	 *
	 * @return the policy
	 */
	<T extends StrolchPolicy> T getPolicy(Class<T> clazz, PolicyDef policyDef, PolicyDef defaultDef);

	/**
	 * Returns the currently set {@link TransactionCloseStrategy}
	 *
	 * @return the currently set {@link TransactionCloseStrategy}
	 */
	TransactionCloseStrategy getCloseStrategy();

	/**
	 * DO NOT CALL THIS METHOD. If the currently set close strategy is {@link TransactionCloseStrategy#READ_ONLY}, then
	 * when the transaction is closed, this method is called no changes to the model is performed but locks on objects
	 * are released and any other resources are released
	 */
	void autoCloseableReadOnly() throws StrolchTransactionException;

	/**
	 * DO NOT CALL THIS METHOD. If the currently set close strategy is {@link TransactionCloseStrategy#COMMIT}, then
	 * when the transaction is closed, this method is called and all registered {@link Command} are performed, locks on
	 * objects are released and any other resources are released
	 */
	void autoCloseableCommit() throws StrolchTransactionException;

	/**
	 * DO NOT CALL THIS METHOD. If the currently set close strategy is {@link TransactionCloseStrategy#ROLLBACK}, then
	 * when the transaction is closed, no further actions are performed and any {@link Command} which were performed
	 * have their {@link Command#undo()} method called and any DB connections are also rolled back
	 */
	void autoCloseableRollback() throws StrolchTransactionException;

	/**
	 * <p>
	 * DO NOT CALL THIS METHOD. This interface implements {@link AutoCloseable} and transactions are expected to be used
	 * in a auto closing try block:
	 * </p>
	 * <p>
	 * <code>
	 * StrolchAgent strolchAgent = getStrolchAgent(); StrolchRealm realm =
	 * strolchAgent.getContainer().getRealm("defaultRealm"); try(StrolchTransaction tx = realm.openTx(certificate,
	 * getClass())){ // do work tx.commitOnClose(); }
	 * </code>
	 * <p>
	 * After the block is closed, the transaction is automatically closed and all allocated resources are released
	 */
	@Override
	void close() throws StrolchTransactionException;

	/**
	 * Sets the {@link TransactionCloseStrategy} to {@link TransactionCloseStrategy#READ_ONLY}
	 */
	StrolchTransaction readOnly();

	/**
	 * Sets the {@link TransactionCloseStrategy} to {@link TransactionCloseStrategy#READ_ONLY}
	 */
	StrolchTransaction doNothingOnClose();

	/**
	 * Sets the {@link TransactionCloseStrategy} to {@link TransactionCloseStrategy#COMMIT}
	 */
	StrolchTransaction commitOnClose();

	/**
	 * Sets the {@link TransactionCloseStrategy} to {@link TransactionCloseStrategy#ROLLBACK}
	 */
	StrolchTransaction rollbackOnClose();

	/**
	 * Sets the {@link TransactionCloseStrategy} to {@link TransactionCloseStrategy#ROLLBACK} and returns a
	 * {@link StrolchTransactionException} which can be thrown by the caller to stop the exception
	 *
	 * @param exceptionMessage
	 * 		the message with which this TX has failed
	 *
	 * @return a {@link StrolchTransactionException} to be thrown by the caller
	 */
	StrolchTransactionException fail(String exceptionMessage);

	/**
	 * Clears the transaction element cache
	 */
	void clearCache();

	/**
	 * Resets this TX so that all caches and changes are reverted, fetched objects might need to be re-fetched to have a
	 * clean state. Locks are not released!
	 */
	void reset();

	/**
	 * <p>
	 * Performs all registered commands
	 * </p>
	 *
	 * <p>
	 * This method does not release any locks, nor does it notify any observers
	 * </p>
	 */
	void flush();

	/**
	 * @return the current state of the transaction
	 *
	 * @see TransactionState
	 */
	TransactionState getState();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionState#OPEN}
	 */
	boolean isOpen();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionCloseStrategy#READ_ONLY} or
	 * {@link TransactionCloseStrategy#ROLLBACK}
	 */
	boolean isReadOnly();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionCloseStrategy#COMMIT}
	 */
	boolean isWriteable();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionState#ROLLING_BACK}
	 */
	boolean isRollingBack();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionState#COMMITTING}
	 */
	boolean isCommitting();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionState#CLOSING}
	 */
	boolean isClosing();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionState#CLOSED}
	 */
	boolean isClosed();

	/**
	 * @return if the current state of the StrolchTransaction is {@link TransactionState#FAILED}
	 */
	boolean isFailed();

	/**
	 * Sets the TX to be silent if the duration is less than the given threshold if the TX was completed successfully
	 *
	 * @param silentThreshold
	 * 		the threshold duration for the TX to be silent if TX duration is less than this value
	 * @param timeUnit
	 * 		the time unit for the given duration
	 */
	StrolchTransaction silentThreshold(long silentThreshold, TimeUnit timeUnit);

	/**
	 * Returns the threshold duration in milliseconds for the TX to be silent if TX duration is less than this value
	 *
	 * @return the threshold duration in milliseconds for the TX to be silent if TX duration is less than this value
	 */
	long getSilentThreshold();

	/**
	 * Suppresses the updates of observer
	 */
	StrolchTransaction suppressUpdates();

	/**
	 * If the given argument is true, then no observer updates are performed
	 *
	 * @param suppressUpdates
	 * 		true to suppress the updates, false to enable them
	 */
	void setSuppressUpdates(boolean suppressUpdates);

	/**
	 * Returns true if the observer updates are currently suppressed
	 *
	 * @return true if the observer updates are currently suppressed
	 */
	boolean isSuppressUpdates();

	/**
	 * If the given argument is true, then no {@link Audit Audits} are written
	 *
	 * @param suppressAudits
	 * 		true to suppress writing {@link Audit Audits}, false to enable them
	 */
	void setSuppressAudits(boolean suppressAudits);

	/**
	 * If the given argument is true, then no {@link Audit Audits} for Audits are written. Since the {@link AuditTrail}
	 * is also audited, {@link Audit Audits} for Audits are generated, this allows to suppress this should that be
	 * required.
	 *
	 * @param suppressAuditsForAudits
	 * 		true to suppress writing {@link Audit Audits}, false to enable them
	 */
	void setSuppressAuditsForAudits(boolean suppressAuditsForAudits);

	/**
	 * Returns true if writing {@link Audit Audits} is currently suppressed
	 *
	 * @return true if writing {@link Audit Audits} is currently suppressed
	 */
	boolean isSuppressAudits();

	/**
	 * Returns true if writing {@link Audit Audits} for Audits is currently suppressed
	 *
	 * @return true if writing {@link Audit Audits} for Audits is currently suppressed
	 */
	boolean isSuppressAuditsForAudits();

	/**
	 * Returns true if versioning is enabled on the {@link StrolchRealm} for which this transaction has been opened
	 *
	 * @return true if versioning is enabled
	 */
	boolean isVersioningEnabled();

	/**
	 * Returns true if this TX has a lock on the given {@link Locator}
	 *
	 * @param locator
	 * 		the {@link Locator} to check
	 *
	 * @return true if this TX has a lock on the given {@link Locator}
	 */
	boolean hasLock(Locator locator) throws StrolchLockException;

	/**
	 * Returns true if this TX has a lock on the given {@link StrolchRootElement}
	 *
	 * @param element
	 * 		the element which to check
	 *
	 * @return true if this TX has a lock on the given {@link Locator}
	 */
	<T extends StrolchRootElement> boolean hasLock(T element) throws StrolchLockException;

	/**
	 * Locks the element with the given locator and registers it on the transaction so the lock is released when the
	 * transaction is closed
	 *
	 * @param locator
	 * 		the {@link Locator} of the element to lock
	 *
	 * @throws StrolchLockException
	 * 		if something goes wrong while locking
	 */
	void lock(Locator locator) throws StrolchLockException;

	/**
	 * Locks the given element and registers it on the transaction so the lock is released when the transaction is
	 * closed
	 *
	 * @param element
	 * 		the element to lock
	 *
	 * @throws StrolchLockException
	 * 		if something goes wrong while locking
	 */
	<T extends StrolchRootElement> void lock(T element) throws StrolchLockException;

	/**
	 * Performs a read-lock on the given element. This means that the given element is locked by calling
	 * {@link #lock(StrolchRootElement)}, then the object is removed from cache, and then the object is read again from
	 * the {@link ElementMap} so that we have the most current version of the element.
	 */
	<T extends StrolchRootElement> T readLock(T element) throws StrolchLockException;

	/**
	 * Releases the lock of the element so that even though the transaction is still open, another thread/transaction
	 * can lock the element
	 *
	 * @param element
	 * 		the element for which the lock is to be released
	 *
	 * @throws StrolchLockException
	 * 		if something goes wrong while unlocking
	 */
	<T extends StrolchRootElement> void releaseLock(T element) throws StrolchLockException;

	/**
	 * Releases the lock of the element with the given {@link Locator} so that even though the transaction is still
	 * open, another thread/transaction can lock the element
	 *
	 * @param locator
	 * 		the {@link Locator} of the element for which the lock is to be released
	 *
	 * @throws StrolchLockException
	 * 		if something goes wrong while unlocking
	 */
	void releaseLock(Locator locator) throws StrolchLockException;

	/**
	 * Adds the given {@link Command} to the transaction. Using this method guarantees that a {@link Command} is
	 * executed properly:
	 * <ul>
	 * <li>{@link Command#validate()}</li>
	 * <li>{@link Command#doCommand()}</li>
	 * </ul>
	 * <p>
	 * and if an exception occurs:
	 * <ul>
	 * <li>{@link Command#undo()}</li>
	 * </ul>
	 *
	 * @param command
	 * 		the command to add
	 */
	void addCommand(Command command);

	/**
	 * Adds the given {@link Command} to the transaction. Using this method guarantees that a {@link Command} is
	 * executed properly:
	 * <ul>
	 * <li>{@link Command#validate()}</li>
	 * <li>{@link Command#doCommand()}</li>
	 * </ul>
	 * <p>
	 * and if an exception occurs:
	 * <ul>
	 * <li>{@link Command#undo()}</li>
	 * </ul>
	 *
	 * @param command
	 * 		the command to add
	 */
	void add(Command command);

	/**
	 * Helper method to create an {@link Audit} with the given arguments. The audit can then be saved by calling
	 * {@link AuditTrail#add(StrolchTransaction, Audit)}
	 *
	 * @param accessType
	 * 		the type of access
	 * @param elementType
	 * 		the element type, i.e. {@link Tags#RESOURCE}, {@link Tags#ORDER}
	 * @param elementSubType
	 * 		the element sub type, e.g. {@link Resource#getType()}
	 * @param id
	 * 		the id of the element audited
	 *
	 * @return the new audit
	 */
	Audit auditFrom(AccessType accessType, String elementType, String elementSubType, String id);

	/**
	 * Helper method to create an {@link Audit} with the given arguments. The audit can then be saved by calling
	 * {@link AuditTrail#add(StrolchTransaction, Audit)}
	 *
	 * @param accessType
	 * 		the type of access
	 * @param element
	 * 		the element from which to to create the audit
	 *
	 * @return the new audit
	 */
	Audit auditFrom(AccessType accessType, StrolchRootElement element);

	/**
	 * <p>
	 * Used to find a {@link StrolchElement} by a {@link Locator}, throwing exception if the element is not found
	 * </p>
	 *
	 * @throws StrolchModelException
	 * 		if the element could not be found
	 * @see #findElement(Locator, boolean)
	 */
	<T extends StrolchElement> T findElement(Locator locator) throws StrolchModelException, ClassCastException;

	/**
	 * <p>
	 * Used to find a {@link StrolchElement} by a {@link Locator}.
	 * </p>
	 *
	 * <p>
	 * A Locator has the form <i>&lt;ObjectClassType&gt;/&lt;Type&gt;/&lt;Id&gt;</i> - this is the least amount of path
	 * elements to find an object. Thus to query a {@link Resource} of type "MyType" and the id "@1" use the following
	 * path: <i>Resourcee/MyType/@1</i>
	 * </p>
	 *
	 * <p>
	 * This method can also be used to find a deeper element, e.g. a specific {@link Parameter} on an
	 * {@link ParameterBag} on an {@link Order}. This would be done as follows: <i>Order/MyType/Bag/@1/myParam</i>
	 * </p>
	 *
	 * @param locator
	 * 		the locator defining the path to the element which is to be found
	 * @param allowNull
	 * 		if element not found, null is returned - presuming Locator has a valid form
	 *
	 * @return the element described by the locator. If {@link Locator} references an element which does not exist, i.e.
	 * an inexistant {@link Resource} or an inexistand {@link Parameter} on a Resource, then a {@link StrolchException}
	 * is thrown
	 *
	 * @throws StrolchModelException
	 * 		if the element could not be found and {@code allowNull} is false
	 * @throws ClassCastException
	 * 		if the querying code is not asking for the correct instance. Do not query a {@link Parameter} if the variable
	 * 		to which the result is to be is stored is a {@link Resource}, etc.
	 */
	<T extends StrolchElement> T findElement(Locator locator, boolean allowNull)
			throws StrolchModelException, ClassCastException;

	/**
	 * <p>Finds a parameter with the {@link StrolchConstants#BAG_PARAMETERS} and @paramKey on the given @element, but
	 * if it does not exists on the element, then it retrieves the elements parent by using the bag
	 * {@link StrolchModelConstants#BAG_RELATIONS} and the param @parentParamKey.</p>
	 *
	 * <p>In Strolch relationships are usually defined on the parameter bag with the id {@link
	 * StrolchModelConstants#BAG_RELATIONS}</p>
	 *
	 * @param element
	 * 		the element on which to search for the parameter
	 * @param parentParamKey
	 * 		the id of the parameter with which to find the parent
	 * @param paramKey
	 * 		the id of the parameter to find
	 *
	 * @return the {@link Optional} with the parameter which was found, following the hierarchy up the chain
	 */
	<U, T extends Parameter<U>> Optional<T> findParameterOnHierarchy(StrolchRootElement element, String parentParamKey,
			String paramKey);

	/**
	 * <p>Finds a parameter with the given @bagKey and @paramKey on the given @element, but if it does not exists
	 * on the element, then it retrieves the elements parent by using the bag
	 * {@link StrolchModelConstants#BAG_RELATIONS} and the param @parentParamKey.</p>
	 *
	 * <p>In Strolch relationships are usually defined on the parameter bag with the id {@link
	 * StrolchModelConstants#BAG_RELATIONS}</p>
	 *
	 * @param element
	 * 		the element on which to search for the parameter
	 * @param parentParamKey
	 * 		the id of the parameter with which to find the parent
	 * @param bagKey
	 * 		the id of the parameter bag where to find the requested parameter
	 * @param paramKey
	 * 		the id of the parameter to find
	 *
	 * @return the {@link Optional} with the parameter which was found, following the hierarchy up the chain
	 */
	<U, T extends Parameter<U>> Optional<T> findParameterOnHierarchy(StrolchRootElement element, String parentParamKey,
			String bagKey, String paramKey);

	/**
	 * Returns a stream of resources for the given types, if empty, streams all possible types
	 *
	 * @param types
	 * 		the types of resources to return in the stream, if empty, streams all possible types
	 *
	 * @return a stream of resources
	 */
	Stream<Resource> streamResources(String... types);

	/**
	 * Returns a stream of orders for the given types, if empty, streams all possible types
	 *
	 * @param types
	 * 		the types of orders to return in the stream, if empty, streams all possible types
	 *
	 * @return a stream of orders
	 */
	Stream<Order> streamOrders(String... types);

	/**
	 * Returns a stream of activities for the given types, if empty, streams all possible types
	 *
	 * @param types
	 * 		the types of activities to return in the stream, if empty, streams all possible types
	 *
	 * @return a stream of activities
	 */
	Stream<Activity> streamActivities(String... types);

	/**
	 * <p>
	 * Returns a copy of the {@link Resource} of Type {@link StrolchModelConstants#TEMPLATE} with the given type as id,
	 * or null if it does not exist
	 * </p>
	 *
	 * <p>
	 * Templates are {@link StrolchRootElement StrolchRootElements} which have the type
	 * {@link StrolchModelConstants#TEMPLATE} and their id is the type of element for which it is a template. For
	 * instance when creating a {@link Resource} of type {@code Person} then having a template with the id
	 * {@code Person} helps creating new Person resources; get the resource and then create a clone:
	 * {@link Resource#getClone()}
	 * </p>
	 *
	 * @param type
	 * 		the id of the {@link Resource} template
	 *
	 * @return the copy of the {@link Resource} template with the given id, or null if it does not exist
	 */
	Resource getResourceTemplate(String type);

	/**
	 * <p>
	 * Returns a copy of the {@link Resource} of Type {@link StrolchModelConstants#TEMPLATE} with the given type as id.
	 * If {@code assertExists} is true, then an exception is thrown if the template does not exist does not exist
	 * </p>
	 *
	 * <p>
	 * Templates are {@link StrolchRootElement StrolchRootElements} which have the type
	 * {@link StrolchModelConstants#TEMPLATE} and their id is the type of element for which it is a template. For
	 * instance when creating a {@link Resource} of type {@code Person} then having a template with the id
	 * {@code Person} helps creating new Person resources; get the resource and then create a clone:
	 * {@link Resource#getClone()}
	 * </p>
	 *
	 * @param type
	 * 		the id of the {@link Resource} template
	 *
	 * @return the copy of the {@link Resource} template with the given id, or if {@code assertExists} is true, then an
	 * exception is thrown if the resource does not exist
	 *
	 * @throws StrolchException
	 * 		if the template does not exist and {@code assertExists} is true
	 */
	Resource getResourceTemplate(String type, boolean assertExists) throws StrolchException;

	/**
	 * <p>Returns the {@link Resource} with the type {@link StrolchConstants#TYPE_CONFIGURATION} and id {@link
	 * StrolchConstants#RES_CONFIGURATION}.</p>
	 *
	 * <p>Should the configuration resource not exist, then it will be created without any parameters, as {@link
	 * Resource#getBoolean(String)} methods et. al. will then just return empty values for their corresponding
	 * types.</p>
	 *
	 * @return the configuration resource
	 */
	Resource getConfiguration();

	/**
	 * <p>Returns the {@link Resource} with the type {@link StrolchConstants#TYPE_CONFIGURATION} and id {@link
	 * StrolchConstants#RES_CONFIGURATION}.</p>
	 *
	 * <p>Should the configuration resource not exist, then it will be created without any parameters, as {@link
	 * Resource#getBoolean(String)} methods et. al. will then just return empty values for their corresponding
	 * types.</p>
	 *
	 * <p>This method first locks the resource's locator, so that we can perform a read lock</p>
	 *
	 * @return the configuration resource
	 */
	Resource lockAndGetConfiguration();

	/**
	 * <p>
	 * Returns a copy of the {@link Order} of Type {@link StrolchModelConstants#TEMPLATE} with the given type as id, or
	 * null if it does not exist
	 * </p>
	 *
	 * <p>
	 * Templates are {@link StrolchRootElement StrolchRootElements} which have the type
	 * {@link StrolchModelConstants#TEMPLATE} and their id is the type of element for which it is a template. For
	 * instance when creating an {@link Order} of type {@code PurchaseOrder} then having a template with the id
	 * {@code PurchaseOrder} helps creating new PurchaseOrder orders; get the order and then create a clone:
	 * {@link Order#getClone()}
	 * </p>
	 *
	 * @param type
	 * 		the id of the {@link Order} template
	 *
	 * @return the copy of the {@link Order} template with the given id, or null if it does not exist
	 */
	Order getOrderTemplate(String type);

	/**
	 * <p>
	 * Returns a copy of the {@link Order} of Type {@link StrolchModelConstants#TEMPLATE} with the given type as id. If
	 * {@code assertExists} is true, then an exception is thrown if the template does not exist does not exist
	 * </p>
	 *
	 * <p>
	 * Templates are {@link StrolchRootElement StrolchRootElements} which have the type
	 * {@link StrolchModelConstants#TEMPLATE} and their id is the type of element for which it is a template. For
	 * instance when creating an {@link Order} of type {@code PurchaseOrder} then having a template with the id
	 * {@code PurchaseOrder} helps creating new PurchaseOrder orders; get the order and then create a clone:
	 * {@link Order#getClone()}
	 * </p>
	 *
	 * @param type
	 * 		the id of the {@link Order} template
	 *
	 * @return the copy of the {@link Order} template with the given id, or if {@code assertExists} is true, then an
	 * exception is thrown if the order does not exist
	 *
	 * @throws StrolchException
	 * 		if the template does not exist and {@code assertExists} is true
	 */
	Order getOrderTemplate(String type, boolean assertExists) throws StrolchException;

	/**
	 * <p>
	 * Returns a copy of the {@link Activity} of Type {@link StrolchModelConstants#TEMPLATE} with the given type as id,
	 * or null if it does not exist
	 * </p>
	 *
	 * <p>
	 * Templates are {@link StrolchRootElement StrolchRootElements} which have the type
	 * {@link StrolchModelConstants#TEMPLATE} and their id is the type of element for which it is a template. For
	 * instance when creating a {@link Activity} of type {@code ToStock} then having a template with the id
	 * {@code ToStock} helps creating new ToStock activities; get the activity and then create a clone:
	 * {@link Activity#getClone()}
	 * </p>
	 *
	 * @param type
	 * 		the id of the {@link Activity} template
	 *
	 * @return the copy of the {@link Activity} template with the given id, or null if it does not exist
	 */
	Activity getActivityTemplate(String type);

	/**
	 * <p>
	 * Returns a copy of the {@link Activity} of Type {@link StrolchModelConstants#TEMPLATE} with the given type as id.
	 * If {@code assertExists} is true, then an exception is thrown if the template does not exist does not exist
	 * </p>
	 *
	 * <p>
	 * Templates are {@link StrolchRootElement StrolchRootElements} which have the type
	 * {@link StrolchModelConstants#TEMPLATE} and their id is the type of element for which it is a template. For
	 * instance when creating a {@link Activity} of type {@code ToStock} then having a template with the id
	 * {@code ToStock} helps creating new ToStock activities; get the activity and then create a clone:
	 * {@link Activity#getClone()}
	 * </p>
	 *
	 * @param type
	 * 		the id of the {@link Activity} template
	 *
	 * @return the copy of the {@link Activity} template with the given id, or if {@code assertExists} is true, then an
	 * exception is thrown if the activity does not exist
	 *
	 * @throws StrolchException
	 * 		if the template does not exist and {@code assertExists} is true
	 */
	Activity getActivityTemplate(String type, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Resource} with the given type and id, or null if it does not exist
	 *
	 * @param type
	 * 		the type of the {@link Resource}
	 * @param id
	 * 		the id of the {@link Resource}
	 *
	 * @return the {@link Resource} with the given type and id, or null if it does not exist
	 */
	Resource getResourceBy(String type, String id);

	/**
	 * Returns the {@link Resource} with the given type and id, or null if it does not exist
	 *
	 * @param type
	 * 		the type of the {@link Resource}
	 * @param id
	 * 		the id of the {@link Resource}
	 * @param assertExists
	 * 		if true, and resource does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the {@link Resource} with the given type and id, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the resource does not exist, and assertExists is true
	 */
	Resource getResourceBy(String type, String id, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Resource} which is referenced by the given {@link StringParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}
	 * and the UOM must be set to the resource's type and the value is the id of the resource
	 *
	 * @param refP
	 * 		the {@link StringParameter} which references a {@link Resource}
	 *
	 * @return the resource referenced by the parameter, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Resource getResourceBy(StringParameter refP) throws StrolchException;

	/**
	 * Returns the {@link Resource} which is referenced by the given {@link StringParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}
	 * and the UOM must be set to the resource's type and the value is the id of the resource
	 *
	 * @param refP
	 * 		the {@link StringParameter} which references a {@link Resource}
	 * @param assertExists
	 * 		if true, and resource does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the resource referenced by the parameter, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter, or if the resource does
	 * 		not exist, and assertExists is true
	 */
	Resource getResourceBy(StringParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Returns all {@link Resource Resources} which are referenced by the given {@link StringListParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}
	 * and the UOM must be set to the resource's type and the value is the id of the resource
	 *
	 * @param refP
	 * 		the {@link StringListParameter} which references a list of {@link Resource Resources}
	 *
	 * @return the resources referenced by the parameter, or the empty list if they do not exist. <b>Note:</b> Any
	 * missing resources are not returned!
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Resource> getResourcesBy(StringListParameter refP) throws StrolchException;

	/**
	 * Returns all {@link Resource Resources} which are referenced by the given {@link StringListParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}
	 * and the UOM must be set to the resource's type and the value is the id of the resource
	 *
	 * @param refP
	 * 		the {@link StringListParameter} which references a list of {@link Resource Resources}
	 * @param assertExists
	 * 		if true, and resource does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the resources referenced by the parameter, or the empty list if they do not exist. <b>Note:</b> Any
	 * missing resources are not returned unless {@code assertExists} is true
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Resource> getResourcesBy(StringListParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Resource} which is referenced by the given {@link StrolchRootElement}. The element must have a
	 * {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a {@link StringParameter} on it
	 * with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF} and the UOM must be set to the resource's type and the
	 * value is the id of the resource to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Resource} through a {@link StringParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @return the resource referenced by the element, or null if the {@link ParameterBag}, {@link StringParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Resource getResourceByRelation(StrolchRootElement element, String refId) throws StrolchException;

	/**
	 * Returns the {@link Resource} which is referenced by the given {@link StrolchRootElement}. The element must have a
	 * {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a {@link StringParameter} on it
	 * with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF} and the UOM must be set to the resource's type and the
	 * value is the id of the resource to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Resource} through a {@link StringParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 * @param assertExists
	 * 		if true, and referenced resource does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the resource referenced by the element, or null if the {@link ParameterBag}, {@link StringParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Resource getResourceByRelation(StrolchRootElement element, String refId, boolean assertExists)
			throws StrolchException;

	/**
	 * Returns all {@link Resource Resources} which are referenced by the given {@link StrolchRootElement}. The element
	 * must have a {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a
	 * {@link StringListParameter} on it with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF} and the UOM must be set to the resource's type and the
	 * value is the id of the resource to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Resource} through a {@link StringListParameter} with
	 * 		the given refId
	 * @param refId
	 * 		the id of the {@link StringListParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @return the resources referenced by the element, or null if the {@link ParameterBag}, {@link StringListParameter}
	 * or referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Resource> getResourcesByRelation(StrolchRootElement element, String refId) throws StrolchException;

	/**
	 * Returns all {@link Resource Resources} which are referenced by the given {@link StrolchRootElement}. The element
	 * must have a {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a
	 * {@link StringListParameter} on it with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF} and the UOM must be set to the resource's type and the
	 * value is the id of the resource to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Resource} through a {@link StringListParameter} with
	 * 		the given refId
	 * @param refId
	 * 		the id of the {@link StringListParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 * @param assertExists
	 * 		if true, and referenced resource does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the resources referenced by the element, or null if the {@link ParameterBag}, {@link StringListParameter}
	 * or referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Resource> getResourcesByRelation(StrolchRootElement element, String refId, boolean assertExists)
			throws StrolchException;

	/**
	 * <p>Returns the {@link Resource} for the given {@link Action}. This is done by getting the resource by {@link
	 * Action#getResourceType()} and {@link Action#getResourceId()}</p>
	 *
	 * <p>Should the resource not exist, then null is returned</p>
	 *
	 * @param action
	 * 		the action for which to return the resoruce
	 *
	 * @return the resource referenced by the action, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the action is null, or something else goes wrong
	 */
	Resource getResourceFor(Action action) throws StrolchException;

	/**
	 * <p>Returns the {@link Resource} for the given {@link Action}. This is done by getting the resource by {@link
	 * Action#getResourceType()} and {@link Action#getResourceId()}</p>
	 *
	 * <p>Should the resource not exist and {@code assertExists} is true, then an exception is thrown, otherwise
	 * null is returned</p>
	 *
	 * @param action
	 * 		the action for which to return the resoruce
	 *
	 * @return the resource referenced by the action. If the resource does not exist and {@code assertExists} is true
	 * then an exception is thrown, otherwise null is returnee
	 *
	 * @throws StrolchException
	 * 		if the action is null and {@code assertExists} is true, or something else goes wrong
	 */
	Resource getResourceFor(Action action, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Activity} with the given type and id, or null if it does not exist
	 *
	 * @param type
	 * 		the type of the {@link Activity}
	 * @param id
	 * 		the id of the {@link Activity}
	 *
	 * @return the {@link Activity} with the given type and id, or null if it does not exist
	 */
	Activity getActivityBy(String type, String id);

	/**
	 * Returns the {@link Activity} with the given type and id, or null if it does not exist
	 *
	 * @param type
	 * 		the type of the {@link Activity}
	 * @param id
	 * 		the id of the {@link Activity}
	 * @param assertExists
	 * 		if true, and activity does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the {@link Activity} with the given type and id, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the activity does not exist, and assertExists is true
	 */
	Activity getActivityBy(String type, String id, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Activity} which is referenced by the given {@link StringParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF}
	 * and the UOM must be set to the activity's type and the value is the id of the activity
	 *
	 * @param refP
	 * 		the {@link StringParameter} which references an {@link Activity}
	 *
	 * @return the activity referenced by the parameter, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Activity getActivityBy(StringParameter refP) throws StrolchException;

	/**
	 * Returns the {@link Activity} which is referenced by the given {@link StringParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF}
	 * and the UOM must be set to the activity's type and the value is the id of the activity
	 *
	 * @param refP
	 * 		the {@link StringParameter} which references an {@link Activity}
	 * @param assertExists
	 * 		if true, and activity does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the activity referenced by the parameter, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter, or if the activity does
	 * 		not exist, and assertExists is true
	 */
	Activity getActivityBy(StringParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Returns all {@link Activity Activities} which are referenced by the given {@link StringListParameter}. A
	 * reference {@link Parameter} must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF} and the UOM must be set to the activity's type and the
	 * value is the id of the activity
	 *
	 * @param refP
	 * 		the {@link StringListParameter} which references a list of {@link Activity Activities}
	 *
	 * @return the activities referenced by the parameter, or the empty list if they do not exist. <b>Note:</b> Any
	 * missing activities are not returned!
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Activity> getActivitiesBy(StringListParameter refP) throws StrolchException;

	/**
	 * Returns all {@link Activity Activities} which are referenced by the given {@link StringListParameter}. A
	 * reference {@link Parameter} must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF} and the UOM must be set to the activity's type and the
	 * value is the id of the activity
	 *
	 * @param refP
	 * 		the {@link StringListParameter} which references a list of {@link Activity Activities}
	 * @param assertExists
	 * 		if true, and activity does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the activities referenced by the parameter, or the empty list if they do not exist. <b>Note:</b> Any
	 * missing activities are not returned unless {@code assertExists} is true
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Activity> getActivitiesBy(StringListParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Activity} which is referenced by the given {@link StrolchRootElement}. The element must have a
	 * {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a {@link StringParameter} on it
	 * with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF} and the UOM must be set to the activity's type and the
	 * value is the id of the activity to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Activity} through a {@link StringParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @return the activity referenced by the element, or null if the {@link ParameterBag}, {@link StringParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Activity getActivityByRelation(StrolchRootElement element, String refId) throws StrolchException;

	/**
	 * Returns the {@link Activity} which is referenced by the given {@link StrolchRootElement}. The element must have a
	 * {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a {@link StringParameter} on it
	 * with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF} and the UOM must be set to the activity's type and the
	 * value is the id of the activity to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Activity} through a {@link StringParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 * @param assertExists
	 * 		if true, and referenced order does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the activity referenced by the element, or null if the {@link ParameterBag}, {@link StringParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Activity getActivityByRelation(StrolchRootElement element, String refId, boolean assertExists)
			throws StrolchException;

	/**
	 * Returns all {@link Activity Activities} which are referenced by the given {@link StrolchRootElement}. The element
	 * must have a {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a
	 * {@link StringListParameter} on it with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF} and the UOM must be set to the activity's type and the
	 * values are the ids of the activities to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Activity} through a {@link StringListParameter} with
	 * 		the given refId
	 * @param refId
	 * 		the id of the {@link StringListParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @return the activity referenced by the element, or null if the {@link ParameterBag}, {@link StringListParameter}
	 * or referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Activity> getActivitiesByRelation(StrolchRootElement element, String refId) throws StrolchException;

	/**
	 * Returns all {@link Activity Activities} which are referenced by the given {@link StrolchRootElement}. The element
	 * must have a {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a
	 * {@link StringListParameter} on it with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ACTIVITY_REF} and the UOM must be set to the activity's type and the
	 * values are the ids of the activities to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Activity} through a {@link StringListParameter} with
	 * 		the given refId
	 * @param refId
	 * 		the id of the {@link StringListParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 * @param assertExists
	 * 		if true, and referenced order does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the activities referenced by the element, or null if the {@link ParameterBag},
	 * {@link StringListParameter} or referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Activity> getActivitiesByRelation(StrolchRootElement element, String refId, boolean assertExists)
			throws StrolchException;

	/**
	 * Returns the {@link Order} with the given type and id, or null if it does not exist
	 *
	 * @param type
	 * 		the type of the {@link Order}
	 * @param id
	 * 		the id of the {@link Order}
	 *
	 * @return the {@link Order} with the given type and id, or null if it does not exist
	 */
	Order getOrderBy(String type, String id);

	/**
	 * Returns the {@link Order} with the given type and id, or null if it does not exist
	 *
	 * @param type
	 * 		the type of the {@link Order}
	 * @param id
	 * 		the id of the {@link Order}
	 * @param assertExists
	 * 		if true, and order does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the {@link Order} with the given type and id, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the order does not exist, and assertExists is true
	 */
	Order getOrderBy(String type, String id, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Order} which is referenced by the given {@link StringParameter}. A reference {@link Parameter}
	 * must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and the UOM must be
	 * set to the order's type and the value is the id of the order
	 *
	 * @param refP
	 * 		the {@link StringParameter} which references an {@link Order}
	 *
	 * @return the order referenced by the parameter, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Order getOrderBy(StringParameter refP) throws StrolchException;

	/**
	 * Returns the {@link Order} which is referenced by the given {@link StringParameter}. A reference {@link Parameter}
	 * must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and the UOM must be
	 * set to the order's type and the value is the id of the order
	 *
	 * @param refP
	 * 		the {@link StringParameter} which references an {@link Order}
	 * @param assertExists
	 * 		if true, and order does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the order referenced by the parameter, or null if it does not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter, or if the order does not
	 * 		exist, and assertExists is true
	 */
	Order getOrderBy(StringParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Returns all {@link Order Orders} which are referenced by the given {@link StringListParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and
	 * the UOM must be set to the order's type and the value is the id of the order
	 *
	 * @param refP
	 * 		the {@link StringListParameter} which references a list of {@link Order Orders}
	 *
	 * @return the orders referenced by the parameter, or the empty list if they do not exist. <b>Note:</b> Any missing
	 * orders are not returned!
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Order> getOrdersBy(StringListParameter refP) throws StrolchException;

	/**
	 * Returns all {@link Order Orders} which are referenced by the given {@link StringListParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and
	 * the UOM must be set to the order's type and the value is the id of the order
	 *
	 * @param refP
	 * 		the {@link StringListParameter} which references a list of {@link Order Orders}
	 * @param assertExists
	 * 		if true, and order does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the orders referenced by the parameter, or the empty list if they do not exist. <b>Note:</b> Any missing
	 * orders are not returned unless {@code assertExists} is true
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Order> getOrdersBy(StringListParameter refP, boolean assertExists) throws StrolchException;

	/**
	 * Returns the {@link Order} which is referenced by the given {@link StrolchRootElement}. The element must have a
	 * {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a {@link StringParameter} on it
	 * with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to the order's type and the value
	 * is the id of the order to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Order} through a {@link StringParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @return the order referenced by the element, or null if the {@link ParameterBag}, {@link StringParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Order getOrderByRelation(StrolchRootElement element, String refId) throws StrolchException;

	/**
	 * Returns the {@link Order} which is referenced by the given {@link StrolchRootElement}. The element must have a
	 * {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a {@link StringParameter} on it
	 * with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to the order's type and the value
	 * is the id of the order to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Order} through a {@link StringParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 * @param assertExists
	 * 		if true, and referenced order does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the order referenced by the element, or null if the {@link ParameterBag}, {@link StringParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	Order getOrderByRelation(StrolchRootElement element, String refId, boolean assertExists) throws StrolchException;

	/**
	 * Returns all {@link Order Orders} which are referenced by the given {@link StrolchRootElement}. The element must
	 * have a {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a
	 * {@link StringListParameter} on it with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to the order's type and the values
	 * are the ids of the orders to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Order} through a {@link StringListParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringListParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @return the orders referenced by the element, or null if the {@link ParameterBag}, {@link StringListParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Order> getOrdersByRelation(StrolchRootElement element, String refId) throws StrolchException;

	/**
	 * Returns all {@link Order Orders} which are referenced by the given {@link StrolchRootElement}. The element must
	 * have a {@link ParameterBag} with the id {@link StrolchModelConstants#BAG_RELATIONS} and a
	 * {@link StringListParameter} on it with the given refId. The parameter must have its interpretation set to
	 * {@link StrolchModelConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to the order's type and the values
	 * are the ids of the orders to return
	 *
	 * @param element
	 * 		the {@link StrolchRootElement} which references a {@link Order} through a {@link StringListParameter} with the
	 * 		given refId
	 * @param refId
	 * 		the id of the {@link StringListParameter} which needs to be on the {@link ParameterBag} with the id
	 *        {@link StrolchModelConstants#BAG_RELATIONS}
	 * @param assertExists
	 * 		if true, and referenced order does not exist, then a {@link StrolchException} is thrown
	 *
	 * @return the orders referenced by the element, or null if the {@link ParameterBag}, {@link StringListParameter} or
	 * referenced element do not exist
	 *
	 * @throws StrolchException
	 * 		if the {@link StringListParameter} is not a properly configured as a reference parameter
	 */
	List<Order> getOrdersByRelation(StrolchRootElement element, String refId, boolean assertExists)
			throws StrolchException;

	/**
	 * Allows to evict a {@link Locator} from the transaction's cache and object filter
	 *
	 * @param locator
	 * 		the locator of the object to remove from cache
	 */
	void removeFromCache(Locator locator);

	/**
	 * Returns true if the given resource is currently cached
	 */
	boolean isResourceCached(String type, String id);

	/**
	 * Returns true if the given order is currently cached
	 */
	boolean isOrderCached(String type, String id);

	/**
	 * Returns true if the given activity is currently cached
	 */
	boolean isActivityCached(String type, String id);

	/**
	 * Returns the cached resource with the given type and id, or null if not yet fetched
	 *
	 * @param type
	 * 		the resource type
	 * @param id
	 * 		the resource id
	 *
	 * @return the cached element, or null if not yet fetched
	 */
	Resource getCachedResource(String type, String id);

	/**
	 * Returns the cached order with the given type and id, or null if not yet fetched
	 *
	 * @param type
	 * 		the order type
	 * @param id
	 * 		the order id
	 *
	 * @return the cached element, or null if not yet fetched
	 */
	Order getCachedOrder(String type, String id);

	/**
	 * Returns the cached activity with the given type and id, or null if not yet fetched
	 *
	 * @param type
	 * 		the activity type
	 * @param id
	 * 		the activity id
	 *
	 * @return the cached element, or null if not yet fetched
	 */
	Activity getCachedActivity(String type, String id);

	/**
	 * Returns a stream of resources in the cache
	 */
	Stream<Resource> streamCachedResources(String... types);

	/**
	 * Returns a stream of orders in the cache
	 */
	Stream<Order> streamCachedOrders(String... types);

	/**
	 * Returns a stream of activities in the cache
	 */
	Stream<Activity> streamCachedActivities(String... types);

	/**
	 * Returns true if the @{@link Resource} exists with the given type and ID
	 *
	 * @param type
	 * 		the type of Resource to check for
	 * @param id
	 * 		the ID of the Resource to check for
	 *
	 * @return true if the @{@link Resource} exists with the given type and ID
	 */
	boolean hasResource(String type, String id);

	/**
	 * Returns true if the @{@link Order} exists with the given type and ID
	 *
	 * @param type
	 * 		the type of Order to check for
	 * @param id
	 * 		the ID of the Order to check for
	 *
	 * @return true if the @{@link Order} exists with the given type and ID
	 */
	boolean hasOrder(String type, String id);

	/**
	 * Returns true if the @{@link Activity} exists with the given type and ID
	 *
	 * @param type
	 * 		the type of Activity to check for
	 * @param id
	 * 		the ID of the Activity to check for
	 *
	 * @return true if the @{@link Activity} exists with the given type and ID
	 */
	boolean hasActivity(String type, String id);

	/**
	 * Returns true if the @{@link Resource} was modified in this TX
	 *
	 * @param resource
	 * 		the resource to check if modified
	 *
	 * @return true if the @{@link Resource} was modified in this TX
	 */
	boolean isUpdated(Resource resource);

	/**
	 * Returns true if the @{@link Order} was modified in this TX
	 *
	 * @param order
	 * 		the order to check if modified
	 *
	 * @return true if the @{@link Order} was modified in this TX
	 */
	boolean isUpdated(Order order);

	/**
	 * Returns true if the @{@link Activity} was modified in this TX
	 *
	 * @param activity
	 * 		the activity to check if modified
	 *
	 * @return true if the @{@link Activity} was modified in this TX
	 */
	boolean isUpdated(Activity activity);

	/**
	 * Returns true if the @{@link Resource} was removed in this TX
	 *
	 * @param resource
	 * 		the resource to check if removed
	 *
	 * @return true if the @{@link Resource} was removed in this TX
	 */
	boolean isRemoved(Resource resource);

	/**
	 * Returns true if the @{@link Order} was removed in this TX
	 *
	 * @param order
	 * 		the order to check if removed
	 *
	 * @return true if the @{@link Order} was removed in this TX
	 */
	boolean isRemoved(Order order);

	/**
	 * Returns true if the @{@link Activity} was removed in this TX
	 *
	 * @param activity
	 * 		the activity to check if removed
	 *
	 * @return true if the @{@link Activity} was removed in this TX
	 */
	boolean isRemoved(Activity activity);

	/**
	 * Add or update and thus persist the given {@link Resource} by calling the relevant {@link Command}
	 *
	 * @param resource
	 * 		the resource to add or update
	 *
	 * @throws StrolchModelException
	 * 		if the resource is null
	 */
	void addOrUpdate(Resource resource) throws StrolchModelException;

	/**
	 * Add or update and thus persist the given {@link Order} by calling the relevant {@link Command}
	 *
	 * @param order
	 * 		the order to add or update
	 *
	 * @throws StrolchModelException
	 * 		if the order is null
	 */
	void addOrUpdate(Order order) throws StrolchModelException;

	/**
	 * Add or update and thus persist the given {@link Activity} by calling the relevant {@link Command}
	 *
	 * @param activity
	 * 		the activity to add or update
	 *
	 * @throws StrolchModelException
	 * 		if the resource is null
	 */
	void addOrUpdate(Activity activity) throws StrolchModelException;

	/**
	 * Adds and thus persists the given {@link Resource} by calling the relevant {@link Command}
	 *
	 * @param resource
	 * 		the resource to add
	 *
	 * @throws StrolchModelException
	 * 		if the resource is null, or a resource with the given ID already exists
	 */
	void add(Resource resource) throws StrolchModelException;

	/**
	 * Adds and thus persists the given {@link Order} by calling the relevant {@link Command}
	 *
	 * @param order
	 * 		the order to add
	 *
	 * @throws StrolchModelException
	 * 		if the order is null, or an order with the given ID already exists
	 */
	void add(Order order) throws StrolchException;

	/**
	 * Adds and thus persists the given {@link Activity} by calling the relevant {@link Command}.
	 *
	 * @param activity
	 * 		the activity to add
	 *
	 * @throws StrolchModelException
	 * 		if the activity is null, or an activity with the given ID already exists
	 */
	void add(Activity activity) throws StrolchException;

	/**
	 * Updates the given {@link Resource} by calling the relevant {@link Command}
	 *
	 * @param resource
	 * 		the resource to update
	 *
	 * @throws StrolchModelException
	 * 		if the resource is null
	 */
	void update(Resource resource) throws StrolchException;

	/**
	 * Updates the given {@link Order} by calling the relevant {@link Command}
	 *
	 * @param order
	 * 		the order to update
	 *
	 * @throws StrolchModelException
	 * 		if the order is null
	 */
	void update(Order order);

	/**
	 * Updates the given {@link Activity} by calling the relevant {@link Command}
	 *
	 * @param activity
	 * 		the activity to update
	 *
	 * @throws StrolchModelException
	 * 		if the activity is null
	 */
	void update(Activity activity) throws StrolchException;

	/**
	 * Removes the given {@link Resource} by calling the relevant {@link Command}
	 *
	 * @param resource
	 * 		the resource to remove
	 *
	 * @throws StrolchModelException
	 * 		if the resource is null
	 */
	void remove(Resource resource) throws StrolchException;

	/**
	 * Removes the given {@link Order} by calling the relevant {@link Command}
	 *
	 * @param order
	 * 		the order to remove
	 *
	 * @throws StrolchModelException
	 * 		if the order is null
	 */
	void remove(Order order) throws StrolchException;

	/**
	 * Removes the given {@link Activity} by calling the relevant {@link Command}
	 *
	 * @param activity
	 * 		the activity to remove
	 *
	 * @throws StrolchModelException
	 * 		if the activity is null
	 */
	void remove(Activity activity) throws StrolchException;

	/**
	 * Return true if this transaction has any registered elements or commands for persistence. E.g. {@link #add(Order)}
	 * or {@link #addCommand(Command)}, etc. were called
	 *
	 * @return true if this transaction needs to be committed to persist changes
	 */
	boolean needsCommit();

	/**
	 * Asserts that the current {@link Certificate} has access to the given element with the given operation
	 *
	 * @param operation
	 * 		the operation to be performed
	 * @param element
	 * 		the element on which the operation is performed
	 *
	 * @throws AccessDeniedException
	 * 		if the session for this TX does not have the required privilege
	 */
	void assertHasPrivilege(Operation operation, StrolchRootElement element) throws AccessDeniedException;

	/**
	 * @see PrivilegeContext#validateAction(String, String)
	 */
	void assertHasPrivilege(String privilegeName, String privilegeValue) throws AccessDeniedException;

	/**
	 * @see PrivilegeContext#validateAction(Restrictable)
	 */
	void assertHasPrivilege(Restrictable restrictable) throws AccessDeniedException;

	/**
	 * @see PrivilegeContext#hasPrivilege(Restrictable)
	 */
	boolean hasPrivilege(String privilegeName, String privilegeValue) throws AccessDeniedException;

	/**
	 * Asserts that the current {@link Certificate} has the given role
	 *
	 * @param roleName
	 * 		the name of the role the user should have
	 *
	 * @throws AccessDeniedException
	 * 		if the session for this TX does not have the given role
	 */
	void assertHasRole(String roleName) throws AccessDeniedException;

	/**
	 * Asserts that the current {@link Certificate} has at least one of the given roles
	 *
	 * @param roleNames
	 * 		the list of roles to check if the session for this TX has at least one of them
	 *
	 * @throws AccessDeniedException
	 * 		if the session for this TX does not have any of the given roles
	 */
	void assertHasAnyRole(String... roleNames) throws AccessDeniedException;

	/**
	 * Returns the statistics of this TX, i.e. the number of elements added, updated and removed in this TX
	 */
	ObjectFilterStatistics getStatistics();
}
