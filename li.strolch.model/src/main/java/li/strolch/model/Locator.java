/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
 * The {@link Locator} is an immutable object and is used to fully qualify the location of an object in the model. It
 * consists of a {@link List} of Strings which starting from the first value, defining the root, with the last item
 * defining the objects id.
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
	public static final String PATH_SEPARATOR = "/"; //$NON-NLS-1$

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
	private Locator(List<String> pathElements) throws StrolchException {
		if (pathElements == null || pathElements.isEmpty()) {
			throw new StrolchException("The path elements may not be null and must contain at least 1 item"); //$NON-NLS-1$
		}
		this.pathElements = Collections.unmodifiableList(new ArrayList<String>(pathElements));
	}

	/**
	 * Constructs a new {@link Locator} using the given path parts
	 *
	 * @param path
	 *            the path to parse for instantiate this {@link Locator} with elements
	 *
	 * @throws StrolchException
	 *             if the path is invalid, meaning has less than two elements in it
	 */
	private Locator(String... path) throws StrolchException {
		this.pathElements = Collections.unmodifiableList(Arrays.asList(path));
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
	private Locator(String path) throws StrolchException {
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
	 * Internal constructor to append a element to a constructor
	 *
	 * @param path
	 *            the base path of the locator
	 * @param element
	 *            the additional element
	 */
	private Locator(List<String> path, String element) {
		List<String> fullPath = new ArrayList<String>();
		fullPath.addAll(path);
		fullPath.add(element);
		this.pathElements = Collections.unmodifiableList(fullPath);
	}

	/**
	 * Returns the immutable list of path elements making up this locator
	 *
	 * @return the pathElements
	 */
	public List<String> getPathElements() {
		return this.pathElements;
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
	 * Returns a new {@link Locator} where the given sub path is appended to the locator
	 *
	 * @param subPathElements
	 *            the sub path to append
	 *
	 * @return the new locator
	 */
	public Locator append(String... subPathElements) {
		return new Locator(this.pathElements, Arrays.asList(subPathElements));
	}

	/**
	 * Returns a new {@link Locator} where the given element is appended to the locator
	 *
	 * @param element
	 *            the element to append
	 *
	 * @return the new locator
	 */
	public Locator append(String element) {
		return new Locator(this.pathElements, element);
	}

	/**
	 * Returns the string representation of this {@link Locator} by using the {@link #PATH_SEPARATOR} to separate the
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
		if (StringHelper.isEmpty(path)) {
			throw new StrolchException("A path may not be empty!"); //$NON-NLS-1$
		}
		String[] elements = path.split(Locator.PATH_SEPARATOR);
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
		StringBuilder sb = new StringBuilder();

		Iterator<String> iter = pathElements.iterator();
		while (iter.hasNext()) {
			String element = iter.next();
			sb.append(element);
			if (iter.hasNext()) {
				sb.append(Locator.PATH_SEPARATOR);
			}
		}

		return sb.toString();
	}

	/**
	 * Returns true if the given locator's path elements is the beginning of this locator's path elements
	 * 
	 * @param locator
	 * 
	 * @return true if the given locator's path elements is the beginning of this locator's path elements
	 */
	public boolean isEqualOrChildOf(Locator locator) {
		if (locator.pathElements.size() > this.pathElements.size())
			return false;
		return this.pathElements.subList(0, locator.pathElements.size()).equals(locator.pathElements);
	}

	/**
	 * Returns true if the given locator's path elements is the beginning of this locator's path elements, but not if
	 * they are the same, i.e. must be an actual child
	 * 
	 * @param locator
	 * 
	 * @return true if the given locator's path elements is the beginning of this locator's path elements, but not if
	 *         they are the same, i.e. must be an actual child
	 */
	public boolean isChildOf(Locator locator) {
		if (locator.pathElements.size() >= this.pathElements.size())
			return false;
		return this.pathElements.subList(0, locator.pathElements.size()).equals(locator.pathElements);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.pathElements == null) ? 0 : this.pathElements.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		Locator other = (Locator) obj;
		if (this.pathElements == null) {
			if (other.pathElements != null) {
				return false;
			}
		} else if (!this.pathElements.equals(other.pathElements)) {
			return false;
		}
		return true;
	}

	/**
	 * Instantiates a new immutable {@link Locator} instance from the given string
	 *
	 * @param locatorPath
	 *            the path from which to instantiate the locator
	 * @return the immutable {@link Locator} instance
	 */
	public static Locator valueOf(String locatorPath) {
		return new Locator(locatorPath);
	}

	/**
	 * Instantiates a new immutable {@link Locator} instance from the given path parts
	 *
	 * @param path
	 *            the path from which to instantiate the locator
	 * @return the immutable {@link Locator} instance
	 */
	public static Locator valueOf(String... path) {
		return new Locator(path);
	}

	/**
	 * Creates a new {@link LocatorBuilder} instance and appends the given elements to it
	 *
	 * @param path
	 *            the first element on the {@link Locator}
	 *
	 * @return a new {@link LocatorBuilder} instance with the given root element tag as the first element
	 */
	public static LocatorBuilder newBuilder(String... path) {
		return new LocatorBuilder().append(path);
	}

	/**
	 * Creates a new {@link LocatorBuilder} instance and appends the given root element tag to it
	 *
	 * @param rootElement
	 *            the first element on the {@link Locator}
	 *
	 * @return a new {@link LocatorBuilder} instance with the given root element tag as the first element
	 */
	public static LocatorBuilder newBuilder(String rootElement) {
		return new LocatorBuilder().append(rootElement);
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
		 * Append the given elements to the path
		 *
		 * @param path
		 *            the path elements to add
		 *
		 * @return this instance for chaining
		 */
		public LocatorBuilder append(String... path) {
			for (String element : path) {
				this.pathElements.add(element);
			}
			return this;
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
		 * Remove the last element from the path
		 *
		 * @return this instance for chaining
		 */
		public LocatorBuilder removeLast() {
			this.pathElements.remove(this.pathElements.size() - 1);
			return this;
		}

		/**
		 * Creates an immutable {@link Locator} instance with the current elements
		 *
		 * @return a new {@link Locator} instance
		 */
		public Locator build() {
			if (this.pathElements.isEmpty()) {
				throw new StrolchException("The path elements must contain at least 1 item"); //$NON-NLS-1$
			}
			return new Locator(this.pathElements);
		}
	}
}
