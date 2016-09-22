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
package li.strolch.runtime.privilege;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.SystemUserAction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.StrolchConstants;

/**
 * The privilege handler for authenticating users and performing actions as a system user
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PrivilegeHandler {

	/**
	 * Authenticate a user
	 * 
	 * @param username
	 *            the username
	 * @param password
	 *            the password
	 * 
	 * @return the certificate
	 * 
	 * @see li.strolch.privilege.handler.PrivilegeHandler#authenticate(String, byte[])
	 */
	public Certificate authenticate(String username, byte[] password);

	/**
	 * Validate that the certificate is still valid
	 * 
	 * @param certificate
	 *            the certificate
	 * 
	 * @throws PrivilegeException
	 *             if the certificate is not valid
	 * 
	 * @see li.strolch.privilege.handler.PrivilegeHandler#isCertificateValid(Certificate)
	 */
	public void isCertificateValid(Certificate certificate) throws PrivilegeException;

	/**
	 * Invalidates the given certificate
	 * 
	 * @param certificate
	 *            the certificate
	 * 
	 * @return true if the certificate was invalidated, or false if it was already invalidated
	 * 
	 * @see li.strolch.privilege.handler.PrivilegeHandler#invalidateSession(li.strolch.privilege.model.Certificate)
	 */
	public boolean invalidateSession(Certificate certificate);

	/**
	 * Notifies that the session has timed out, i.e. the certificate must be invalidated
	 * 
	 * @param certificate
	 *            the certificate that has timed out
	 * @return true if the certificate was invalidated, or false it was already invalidated
	 * 
	 * @see li.strolch.privilege.handler.PrivilegeHandler#invalidateSession(li.strolch.privilege.model.Certificate)
	 */
	public boolean sessionTimeout(Certificate certificate);

	/**
	 * Returns the {@link PrivilegeContext} for the given certificate
	 * 
	 * @param certificate
	 *            the certificate
	 * 
	 * @return the {@link PrivilegeContext} for the given certificate
	 * 
	 * @throws PrivilegeException
	 *             if the certificate is not valid anymore
	 * 
	 * @see li.strolch.privilege.handler.PrivilegeHandler#getPrivilegeContext(li.strolch.privilege.model.Certificate)
	 */
	public PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException;

	/**
	 * Run the given {@link SystemUserAction} as the given system user
	 * 
	 * @param systemUsername
	 *            the system username
	 * @param action
	 *            the action to perform
	 * 
	 * @return the action after performing the action
	 * 
	 * @throws PrivilegeException
	 *             if there is something wrong
	 * 
	 * @see li.strolch.privilege.handler.PrivilegeHandler#runAsSystem(java.lang.String,
	 *      li.strolch.privilege.handler.SystemUserAction)
	 */
	public <T extends SystemUserAction> T runAsSystem(String systemUsername, T action) throws PrivilegeException;

	/**
	 * Run the given {@link SystemUserAction} as the system user {@link StrolchConstants#PRIVILEGED_SYSTEM_USER}
	 * 
	 * @param action
	 *            the action to perform
	 * 
	 * @return the action after performing the action
	 * 
	 * @throws PrivilegeException
	 *             if there is something wrong
	 */
	public <V extends SystemUserAction> V runPrivileged(V action) throws PrivilegeException;

	/**
	 * Returns the {@link li.strolch.privilege.handler.PrivilegeHandler}
	 * 
	 * @return the {@link li.strolch.privilege.handler.PrivilegeHandler}
	 */
	public abstract li.strolch.privilege.handler.PrivilegeHandler getPrivilegeHandler();

}