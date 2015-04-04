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

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PrivilegeHandler {

	public static final String PRIVILEGE = "Privilege"; //$NON-NLS-1$
	public static final String CERTIFICATE = "Certificate"; //$NON-NLS-1$

	/**
	 * @param username
	 * @param password
	 * @return
	 * 
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#authenticate(String, byte[])
	 */
	public abstract Certificate authenticate(String username, byte[] password);

	/**
	 * @param certificate
	 * @throws PrivilegeException
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#isCertificateValid(Certificate)
	 */
	public abstract void isCertificateValid(Certificate certificate) throws PrivilegeException;

	/**
	 * @param certificate
	 * @return
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#invalidateSession(ch.eitchnet.privilege.model.Certificate)
	 */
	public abstract boolean invalidateSession(Certificate certificate);

	/**
	 * @param certificate
	 * @return
	 * @throws PrivilegeException
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getPrivilegeContext(ch.eitchnet.privilege.model.Certificate)
	 */
	public abstract PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException;

	/**
	 * @param systemUsername
	 * @param action
	 * @throws PrivilegeException
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#runAsSystem(java.lang.String,
	 *      ch.eitchnet.privilege.handler.SystemUserAction)
	 */
	public abstract void runAsSystem(String systemUsername, SystemUserAction action) throws PrivilegeException;

	/**
	 * @param certificate
	 * @return
	 * @throws PrivilegeException
	 */
	public abstract ch.eitchnet.privilege.handler.PrivilegeHandler getPrivilegeHandler(Certificate certificate)
			throws PrivilegeException;

	/**
	 * @param certificate
	 * @param password
	 * @throws PrivilegeException
	 * @see {@link ch.eitchnet.privilege.handler.PrivilegeHandler#checkPassword(Certificate, byte[])}
	 */
	public void checkPassword(Certificate certificate, byte[] password) throws PrivilegeException;
}