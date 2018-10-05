/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.model.timevalue.impl;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchValueType;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

/**
 * {@link IValue} implementation to work with String valued {@link ITimeValue} objects. Since a java.util.String object
 * does not define a inverse, a algebraic {@link AString} wrapper is used.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class StringSetValue implements IValue<Set<AString>>, Serializable {

	private static Set<AString> neu = Collections.emptySet();
	public static final IValue<Set<AString>> NEUTRAL = new StringSetValue(neu);

	private Set<AString> aStrings = new HashSet<>();

	private StringSetValue() {
	}

	public StringSetValue(final Set<AString> aStrings) {
		DBC.PRE.assertNotNull("Value may not be null!", aStrings);
		this.aStrings = aStrings;
	}

	public StringSetValue(String valueAsString) {
		Set<AString> value = new HashSet<>();
		String[] values = valueAsString.split(",");
		for (String s : values) {
			value.add(new AString(s.trim()));
		}
		this.aStrings = value;
	}

	@Override
	public String getType() {
		return StrolchValueType.STRING_SET.getType();
	}

	@Override
	public Set<AString> getValue() {
		return this.aStrings;
	}

	@Override
	public IValue<Set<AString>> add(Set<AString> o) {

		Set<AString> toBeAdded = new HashSet<>(o);

		for (Iterator<AString> iter1 = toBeAdded.iterator(); iter1.hasNext(); ) {
			AString toAdd = iter1.next();
			if (StringHelper.isEmpty(toAdd.getString())) {
				throw new StrolchException("StringSetValue may not contain null values in set!");
			}

			for (Iterator<AString> iter = this.aStrings.iterator(); iter.hasNext(); ) {
				AString aString = iter.next();
				boolean valueMatch = aString.getString().equals(toAdd.getString());
				boolean compensate =
						(toAdd.isInverse() && !aString.isInverse()) || (!toAdd.isInverse() && aString.isInverse());
				if (valueMatch && compensate) {
					iter.remove();
					iter1.remove();
				}
			}
		}
		this.aStrings.addAll(toBeAdded);
		return this;
	}

	@Override
	public boolean matches(IValue<Set<AString>> other) {
		return getValue().equals(other.getValue());
	}

	@Override
	public StringSetValue getInverse() {
		Set<AString> inverseSet = new HashSet<>();
		for (AString as : this.aStrings) {
			inverseSet.add(as.getInverse());
		}
		StringSetValue inverse = new StringSetValue();
		inverse.aStrings = inverseSet;
		return inverse;
	}

	@Override
	public StringSetValue getCopy() {
		return new StringSetValue(new HashSet<>(this.aStrings));
	}

	@Override
	public String getValueAsString() {
		if (this.aStrings.isEmpty())
			return StringHelper.EMPTY;
		if (this.aStrings.size() == 1)
			return this.aStrings.iterator().next().getString();

		StringBuilder sb = new StringBuilder();
		Iterator<AString> iter = this.aStrings.iterator();
		while (iter.hasNext()) {
			sb.append(iter.next().getString());
			if (iter.hasNext())
				sb.append(", ");
		}
		return sb.toString();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("StringSetValue [aStrings=");
		sb.append(getValueAsString());
		sb.append("]");
		return sb.toString();
	}
}
