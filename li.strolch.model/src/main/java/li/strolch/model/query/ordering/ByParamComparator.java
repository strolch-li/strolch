/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 */
package li.strolch.model.query.ordering;

import java.util.Comparator;

import li.strolch.exception.StrolchException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.parameter.Parameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ByParamComparator<T extends GroupedParameterizedElement> implements Comparator<T> {

	private String bagKey;
	private String paramKey;
	private boolean ascending;

	public ByParamComparator(String bagKey, String paramKey, boolean ascending) {
		this.bagKey = bagKey;
		this.paramKey = paramKey;
		this.ascending = ascending;
	}

	@Override
	public int compare(T o1, T o2) {

		Parameter<?> param1 = o1.getParameter(bagKey, paramKey);
		if (param1 == null)
			throw new StrolchException("Sorting parameter bag=" + bagKey + ", param=" + paramKey
					+ " does not exist on " + o1.getLocator());

		Parameter<?> param2 = o2.getParameter(bagKey, paramKey);
		if (param2 == null)
			throw new StrolchException("Sorting parameter bag=" + bagKey + ", param=" + paramKey
					+ " does not exist on " + o2.getLocator());

		return this.ascending ? param1.compareTo(param2) : param2.compareTo(param1);
	}
}
