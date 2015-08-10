/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 */
package li.strolch.model.query.ordering;

import java.util.Comparator;

import li.strolch.model.GroupedParameterizedElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ByIdComparator<T extends GroupedParameterizedElement> implements Comparator<T> {

	private boolean ascending;

	public ByIdComparator(boolean ascending) {
		this.ascending = ascending;
	}

	@Override
	public int compare(T o1, T o2) {
		return this.ascending ? o1.getId().compareTo(o2.getId()) : o2.getId().compareTo(o1.getId());
	}
}
