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

import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.User;

/**
 * @author rvonburg
 * 
 */
public interface PolicyHandler {

	public boolean actionAllowed(User user, Restrictable restrictable);
}
