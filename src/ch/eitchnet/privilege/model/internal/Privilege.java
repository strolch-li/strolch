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

import java.util.Collections;
import java.util.List;

/**
 * @author rvonburg
 * 
 */
public class Privilege {

	private final boolean allAllowed;
	private final List<String> valuesAllowed;
	private final List<String> valuesNotAllowed;

	/**
	 * @param allAllowed
	 * @param valuesAllowed
	 * @param valuesNotAllowed
	 */
	public Privilege(boolean allAllowed, List<String> valuesAllowed, List<String> valuesNotAllowed) {
		this.allAllowed = allAllowed;
		this.valuesAllowed = Collections.unmodifiableList(valuesAllowed);
		this.valuesNotAllowed = Collections.unmodifiableList(valuesNotAllowed);
	}

	/**
	 * @return the allAllowed
	 */
	public boolean isAllAllowed() {
		return allAllowed;
	}

	/**
	 * @return the valuesAllowed
	 */
	public List<String> getValuesAllowed() {
		return valuesAllowed;
	}

	/**
	 * @return the valuesNotAllowed
	 */
	public List<String> getValuesNotAllowed() {
		return valuesNotAllowed;
	}
}
