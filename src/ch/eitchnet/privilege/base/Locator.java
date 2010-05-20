/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.base;

import java.io.Serializable;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

/**
 * Locator to access RSP objects by a generalized path similar to locator used for accessing files in file systems or
 * URL.
 * <p>
 * The locator is made of the following elements:
 * <li>A identifier of the map to take the top level object from (Resource, Order, Workflow or Script)
 * <li>The id of the top level object
 * <li>The sequence of id's of the elements to navigate to the object
 * <p>
 * It's string representation is similar to a UNIX path. For example a string version if the Locator of a parameter
 * (with id = p1) of the bounds of a resource takes the form: /Resource/Bounds/p1.
 * 
 * @author msmock
 */
public class Locator implements Serializable {

	private static final char LOCATOR_SEPARATOR = '/';
	private static final String LOCATOR_SEPARATOR_S = "/";
	private static final long serialVersionUID = 1L;

	// the path with string entries
	private List<String> path;

	/**
	 * simple constructor
	 */
	public Locator() {
		path = new ArrayList<String>();
	}

	/**
	 * @param path
	 */
	public Locator(List<String> path) {
		this.path = path;
	}

	/**
	 * @param s
	 */
	public Locator(String s) {

		// parse the key to the list of path elements
		s.trim();
		String[] pathElements = s.split(LOCATOR_SEPARATOR_S);
		path = new ArrayList<String>();

		// for the case that we received a non locator string, just set it as
		// the first path, helps while debugging
		if (pathElements.length == 1) {
			path.add(s);
		} else {

			for (int i = 1; i < pathElements.length; i++) {
				path.add(pathElements[i]);
			}
		}
	}

	/**
	 * @return the path
	 */
	public List<String> getPath() {
		return path;
	}

	/**
	 * @param path
	 *            the path to set
	 */
	public void setPath(List<String> path) {
		this.path = path;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		StringWriter sw = new StringWriter();
		for (String s : path) {
			sw.append(LOCATOR_SEPARATOR);
			sw.append(s);
		}
		return sw.toString();
	}

	/**
	 * @param s
	 */
	public void append(String s) {
		path.add(s);
	}

	/**
	 * @return ListIterator<String>
	 */
	public ListIterator<String> iterator() {
		return path.listIterator();
	}

	/**
	 * get the number of path element strings
	 * 
	 * @return int
	 */
	public int length() {
		return path.size();
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object other) {

		if (this == other)
			return true;

		if (!(other instanceof Locator))
			return false;

		final Locator that = (Locator) other;

		// return false, if the size does not match
		if (path.size() != that.path.size())
			return false;

		// compare the path elements
		for (int i = 0; i < path.size(); i++) {
			String thisS = path.get(i);
			String thatS = that.path.get(i);
			if (!thisS.equals(thatS))
				return false;
		}

		// all path elements match
		return true;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return toString().hashCode();
	}

}
