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
package ch.eitchnet.featherlite.plugin.xmlpers.test.impl;

import ch.eitchnet.xmlpers.XmlDao;
import ch.eitchnet.xmlpers.XmlDaoFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class MyDaoFactory implements XmlDaoFactory {

	/**
	 * @see ch.eitchnet.xmlpers.XmlDaoFactory#getDao(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T> XmlDao<T> getDao(String type) {
		if (type.equals(MyClass.class.getName()))
			return (XmlDao<T>) new MyClassDao();

		throw new RuntimeException("Class with type " + type + " is unknown!");
	}

}
