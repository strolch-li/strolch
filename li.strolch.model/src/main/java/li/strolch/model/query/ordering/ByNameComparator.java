/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 */
package li.strolch.model.query.ordering;

import java.util.Comparator;

import li.strolch.model.GroupedParameterizedElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ByNameComparator<T extends GroupedParameterizedElement> implements Comparator<T> {

	private boolean ascending;

	public ByNameComparator(boolean ascending) {
		this.ascending = ascending;
	}

	@Override
	public int compare(T o1, T o2) {
		return this.ascending ? o1.getName().compareTo(o2.getName()) : o2.getName().compareTo(o1.getName());
	}
}
