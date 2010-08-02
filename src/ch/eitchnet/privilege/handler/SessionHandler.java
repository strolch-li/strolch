/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.handler;

import ch.eitchnet.privilege.base.PrivilegeContainerObject;
import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.User;

/**
 * @author rvonburg
 * 
 */
public interface SessionHandler extends PrivilegeContainerObject {

	/**
	 * @param certificate
	 * @param restrictable
	 * 
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User} or if the user may not
	 *             perform the action defined by the {@link Restrictable} implementation
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public boolean actionAllowed(Certificate certificate, Restrictable restrictable);

	/**
	 * @param certificate
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User}
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public boolean isCertificateValid(Certificate certificate);

	/**
	 * @param username
	 * @param password
	 * 
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	public Certificate authenticate(String username, String password);
}
