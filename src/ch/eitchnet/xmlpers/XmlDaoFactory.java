/*
 * Copyright (c) 2010 - 2011
 * 
 * Apixxo AG 
 * Hauptgasse 25
 * 4600 Olten
 * 
 * All rights reserved.
 * 
 */
package ch.eitchnet.xmlpers;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public interface XmlDaoFactory {

	public <T> XmlDao<T> getDao(String type);
}
