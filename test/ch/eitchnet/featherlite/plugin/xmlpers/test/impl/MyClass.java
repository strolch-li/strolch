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

import ch.eitchnet.utils.objectfilter.ITransactionObject;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class MyClass implements ITransactionObject {

	private long txId;
	private String id;
	private String name;
	private String type;

	/**
	 * @param id
	 * @param name
	 * @param type
	 */
	public MyClass(String id, String name, String type) {
		super();
		this.id = id;
		this.name = name;
		this.type = type;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return this.type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * @see ch.eitchnet.utils.objectfilter.ITransactionObject#setTransactionID(long)
	 */
	@Override
	public void setTransactionID(long id) {
		this.txId = id;
	}

	/**
	 * @see ch.eitchnet.utils.objectfilter.ITransactionObject#getTransactionID()
	 */
	@Override
	public long getTransactionID() {
		return this.txId;
	}

	/**
	 * @see ch.eitchnet.utils.objectfilter.ITransactionObject#resetTransactionID()
	 */
	@Override
	public void resetTransactionID() {
		this.txId = ITransactionObject.UNSET;
	}
}
