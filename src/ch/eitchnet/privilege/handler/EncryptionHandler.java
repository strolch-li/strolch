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

/**
 * @author rvonburg
 * 
 */
public interface EncryptionHandler extends PrivilegeContainerObject{

	public String nextToken();

	public String convertToHash(String string);
}
