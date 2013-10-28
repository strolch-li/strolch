/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.privilege;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface StrolchPrivilegeHandler {

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
	public abstract PrivilegeHandler getPrivilegeHandler(Certificate certificate) throws PrivilegeException;

}