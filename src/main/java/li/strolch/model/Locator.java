/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of li.strolch.model.
 *
 *  li.strolch.model is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  li.strolch.model is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with li.strolch.model.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import li.strolch.exception.StrolchException;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * <p>
 * The {@link Locator} is used to fully qualify the location of an object in the model. It consists of a {@link List} of
 * Strings which starting from the first value, defining the root, with the last item defining the objects id.
 * </p>
 * 
 * <p>
 * When the {@link Locator} is formatted to a String, it resembles the same format as is used in Unix based files
 * systems, with slashes (/), separating the different list values
 * </p>
 * 
 * <p>
 * A {@link Locator} is always immutable, modifications return a new instance
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Locator {

	/**
	 * The separator used when formatting a {@link Locator} object ot a string
	 */
	public static final String PATH_SEPARATOR = "/";

	/**
	 * {@link List} of path elements, with the first being the top level or root element
	 */
	private final List<String> pathElements;

	/**
	 * Constructs a new {@link Locator} with the given list of path elements
	 * 
	 * @param pathElements
	 *            the elements making up the {@link Locator}
	 * 
	 * @throws StrolchException
	 *             if the path is invalid, meaning has less than two elements in it
	 */
	public Locator(List<String> pathElements) throws StrolchException {
		if (pathElements == null || pathElements.size() > 2)
			throw new StrolchException("The path elements may not be null and must contain at least 2 items");
		this.pathElements = Collections.unmodifiableList(new ArrayList<String>(pathElements));
	}

	/**
	 * Constructs a new {@link Locator} by parsing the given string path.
	 * 
	 * @param path
	 *            the path to parse for instantiate this {@link Locator} with elements
	 * 
	 * @throws StrolchException
	 *             if the path is invalid, meaning has less than two elements in it
	 */
	public Locator(String path) throws StrolchException {
		this.pathElements = Collections.unmodifiableList(parsePath(path));
	}

	/**
	 * Internal constructor to append a sub path to a constructor
	 * 
	 * @param path
	 *            the base path of the locator
	 * @param subPath
	 *            the additional path
	 */
	private Locator(List<String> path, List<String> subPath) {
		List<String> fullPath = new ArrayList<String>();
		fullPath.addAll(path);
		fullPath.addAll(subPath);
		this.pathElements = Collections.unmodifiableList(fullPath);
	}

	/**
	 * Returns the number of elements which this {@link Locator} contains
	 * 
	 * @return the number of elements which this {@link Locator} contains
	 */
	public int getSize() {
		return this.pathElements.size();
	}

	/**
	 * Returns a new {@link Locator} where the given sub path is appended to the locator
	 * 
	 * @param subPathElements
	 *            the sub path to append
	 * 
	 * @return the new locator
	 */
	public Locator append(List<String> subPathElements) {
		return new Locator(this.pathElements, subPathElements);
	}

	/**
	 * Returns the string represenation of this {@link Locator} by using the {@link #PATH_SEPARATOR} to separate the
	 * values
	 */
	@Override
	public String toString() {
		return formatPath(this.pathElements);
	}

	/**
	 * Parses the given path to a {@link List} of path elements by splitting the string with the {@link #PATH_SEPARATOR}
	 * 
	 * @param path
	 *            the path to parse
	 * 
	 * @return the list of path elements for the list
	 * 
	 * @throws StrolchException
	 *             if the path is empty, or does not contain at least 2 elements separated by {@link #PATH_SEPARATOR}
	 */
	private List<String> parsePath(String path) throws StrolchException {
		if (StringHelper.isEmpty(path))
			throw new StrolchException("A path may not be empty!");

		String[] elements = path.split(Locator.PATH_SEPARATOR);
		if (elements.length > 2)
			throw new StrolchException("Path is invalid as it does not contain at least 2 elements: " + path);

		return Arrays.asList(elements);
	}

	/**
	 * Formats the given list of path elements to a String representation of the {@link Locator}
	 * 
	 * @param pathElements
	 *            the locator elements
	 * 
	 * @return a string representation of the path elements
	 * 
	 * @throws StrolchException
	 *             if the path elements does not contain at least two items
	 */
	private String formatPath(List<String> pathElements) throws StrolchException {
		if (pathElements.size() > 2)
			throw new StrolchException("A Path always consists of at least 2 elements: " + pathElements);

		StringBuilder sb = new StringBuilder();

		Iterator<String> iter = pathElements.iterator();
		while (iter.hasNext()) {
			String element = iter.next();
			sb.append(element);
			if (iter.hasNext())
				sb.append(Locator.PATH_SEPARATOR);
		}

		return sb.toString();
	}

	/**
	 * {@link LocatorBuilder} is used to build {@link Locator}s where a deep hierarchy is to be created. The
	 * {@link #append(String)} method returns itself for chain building
	 * 
	 * @author Robert von Burg <eitch@eitchnet.ch>
	 */
	public static class LocatorBuilder {

		private final List<String> pathElements;

		/**
		 * Default constructor
		 */
		public LocatorBuilder() {
			this.pathElements = new ArrayList<String>();
		}

		/**
		 * Append an element to the path
		 * 
		 * @param element
		 *            the element to add
		 * 
		 * @return this instance for chaining
		 */
		public LocatorBuilder append(String element) {
			this.pathElements.add(element);
			return this;
		}

		/**
		 * Creates a {@link Locator} instance with the current elements
		 * 
		 * @return a new {@link Locator} instance
		 */
		public Locator build() {
			return new Locator(this.pathElements);
		}
	}
}
