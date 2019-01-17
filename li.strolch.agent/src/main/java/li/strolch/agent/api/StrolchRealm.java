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

import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.Locator;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;

/**
 * <p>
 * A {@link StrolchRealm} implement mandate separation. Each realm has its own {@link ElementMap ElementMaps} and can be
 * configured separately to other realms. So cached realms and transient realms can reside in the same agent.
 * </p>
 * 
 * <p>
 * A realm can support the following functionality:
 * </p>
 * <ul>
 * <li>opening of transactions (mandator)</li>
 * <li>locking of objects (mandatory)</li>
 * <li>different modes see {@link DataStoreMode}</li>
 * <li>audit trail for write</li>
 * <li>audit trail for read</li>
 * <li>observer updates</li>
 * <li>element versioning</li>
 * </ul>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchRealm {

	/**
	 * Returns the name of the realm
	 * 
	 * @return the name of the realm
	 */
	String getRealm();

	/**
	 * Locks the element with the given {@link Locator}
	 * 
	 * @param locator
	 *            the locator of the element to lock
	 * 
	 * @see LockHandler#lock(Locator)
	 */
	void lock(Locator locator);

	/**
	 * Unlocks the element with the given {@link Locator} (lock might still be held, if lock counter is used)
	 * 
	 * @param locator
	 *            the locator of the element to unlock
	 * 
	 * @see LockHandler#unlock(Locator)
	 */
	void unlock(Locator locator);

	/**
	 * Releases the lock for the given element
	 * 
	 * @param locator
	 *            the locator of the element for which to release the lock
	 * 
	 * @see LockHandler#releaseLock(Locator)
	 */
	void releaseLock(Locator locator);

	/**
	 * Returns the {@link DataStoreMode}
	 * 
	 * @return the {@link DataStoreMode}
	 */
	DataStoreMode getMode();

	/**
	 * Opens a {@link StrolchTransaction} for the given certificate
	 * 
	 * @param certificate
	 *            the authenticated certificate
	 * @param clazz
	 *            to give the transaction an action name
	 * 
	 * @return the newly created transaction
	 */
	StrolchTransaction openTx(Certificate certificate, Class<?> clazz);

	/**
	 * Opens a {@link StrolchTransaction} for the given certificate
	 * 
	 * @param certificate
	 *            the authenticated certificate
	 * @param action
	 *            to give the transaction an action name
	 * 
	 * @return the newly created transaction
	 */
	StrolchTransaction openTx(Certificate certificate, String action);

	/**
	 * Returns if the audit trail is enabled for reads
	 * 
	 * @return if the audit trail is enabled for reads
	 */
	boolean isAuditTrailEnabledForRead();

	/**
	 * Returns if the audit trail is enabled for modifications
	 * 
	 * @return if the audit trail is enabled for modifications
	 */
	boolean isAuditTrailEnabled();

	/**
	 * Returns if observer updates is enabled
	 * 
	 * @return if observer updates is enabled
	 */
	boolean isUpdateObservers();

	/**
	 * Returns if versioning is enabled
	 * 
	 * @return if versioning is enabled
	 */
	boolean isVersioningEnabled();

	/**
	 * Returns the {@link ObserverHandler} if observer updates are enabled
	 * 
	 * @return the {@link ObserverHandler} if observer updates are enabled
	 * 
	 * @throws IllegalArgumentException
	 *             if observer updates are not enabled
	 */
	ObserverHandler getObserverHandler() throws IllegalArgumentException;
}
