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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PrivilegeHandler {

	/**
	 * @param username
	 * @param password
	 * @return
	 * 
	 * @see li.strolch.privilege.handler.PrivilegeHandler#authenticate(String, byte[])
	 */
	public abstract Certificate authenticate(String username, byte[] password);

	/**
	 * @param certificate
	 * @throws PrivilegeException
	 * @see li.strolch.privilege.handler.PrivilegeHandler#isCertificateValid(Certificate)
	 */
	public abstract void isCertificateValid(Certificate certificate) throws PrivilegeException;

	/**
	 * @param certificate
	 * @return
	 * @see li.strolch.privilege.handler.PrivilegeHandler#invalidateSession(li.strolch.privilege.model.Certificate)
	 */
	public abstract boolean invalidateSession(Certificate certificate);

	/**
	 * @param certificate
	 * @return
	 * @see li.strolch.privilege.handler.PrivilegeHandler#invalidateSession(li.strolch.privilege.model.Certificate)
	 */
	public abstract boolean sessionTimeout(Certificate certificate);

	/**
	 * @param certificate
	 * @return
	 * @throws PrivilegeException
	 * @see li.strolch.privilege.handler.PrivilegeHandler#getPrivilegeContext(li.strolch.privilege.model.Certificate)
	 */
	public abstract PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException;

	/**
	 * @param systemUsername
	 * @param action
	 * @throws PrivilegeException
	 * @see li.strolch.privilege.handler.PrivilegeHandler#runAsSystem(java.lang.String,
	 *      li.strolch.privilege.handler.SystemUserAction)
	 */
	public abstract <T extends SystemUserAction> T runAsSystem(String systemUsername, T action)
			throws PrivilegeException;

	/**
	 * @param certificate
	 * @return
	 * @throws PrivilegeException
	 */
	public abstract li.strolch.privilege.handler.PrivilegeHandler getPrivilegeHandler(Certificate certificate)
			throws PrivilegeException;

	/**
	 * @param certificate
	 * @param password
	 * @throws PrivilegeException
	 * @see {@link li.strolch.privilege.handler.PrivilegeHandler#checkPassword(Certificate, byte[])}
	 */
	public void checkPassword(Certificate certificate, byte[] password) throws PrivilegeException;
}