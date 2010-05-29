/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model.internal;

import ch.eitchnet.privilege.model.Restrictable;

/**
 * @author rvonburg
 * 
 */
public interface RestrictionPolicy {

	public boolean actionAllowed(Role role, Restrictable restrictable);
}
