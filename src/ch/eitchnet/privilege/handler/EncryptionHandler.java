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

import org.dom4j.Element;

/**
 * @author rvonburg
 * 
 */
public interface EncryptionHandler {

	public String nextToken();

	public String convertToHash(String string);

	public void initialize(Element element);
}
