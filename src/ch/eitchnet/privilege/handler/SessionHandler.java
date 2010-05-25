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

import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.User;

/**
 * @author rvonburg
 * 
 */
public interface SessionHandler {

	/**
	 * @param certificate
	 * @param restrictable
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User} or if the user may not
	 *             perform the action defined by the {@link Restrictable} implementation
	 */
	public boolean actionAllowed(Certificate certificate, Restrictable restrictable);

	/**
	 * @param user
	 * @param password
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	public Certificate authenticate(String user, String password);
}
