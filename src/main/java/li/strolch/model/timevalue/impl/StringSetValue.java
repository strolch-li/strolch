package li.strolch.model.timevalue.impl;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;

/**
 * {@link IValue} implementation to work with String valued {@link ITimeValue}
 * objects. Since a java.util.String object does not define a inverse, a
 * algebraic {@link AString} wrapper is used.
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class StringSetValue implements IValue<Set<AString>>, Serializable {

	private static final long serialVersionUID = 1L;

	private static Set<AString> neu = Collections.emptySet();
	public static final IValue<Set<AString>> NEUTRAL = new StringSetValue(neu);

	private Set<AString> aStrings = new HashSet<AString>();

	public StringSetValue() {
	}

	public StringSetValue(final Set<AString> aStrings) {
		this.aStrings = aStrings;
	}

	@Override
	public Set<AString> getValue() {
		return aStrings;
	}

	@Override
	public IValue<Set<AString>> add(Set<AString> o) {

		Set<AString> toBeAdded = new HashSet<AString>(o);

		for (Iterator<AString> iter1 = toBeAdded.iterator(); iter1.hasNext();) {
			AString toAdd = iter1.next();
			for (Iterator<AString> iter = aStrings.iterator(); iter.hasNext();) {
				AString aString = iter.next();
				boolean valueMatch = aString.getString().equals(toAdd.getString());
				boolean compensate = (toAdd.isInverse() && !aString.isInverse())
						|| (!toAdd.isInverse() && aString.isInverse());
				if (valueMatch && compensate) {
					iter.remove();
					iter1.remove();
				}
			}
		}
		aStrings.addAll(toBeAdded);
		return this;
	}

	@Override
	public boolean matches(IValue<Set<AString>> other) {
		return this.getValue().equals(other.getValue());
	}

	@Override
	public IValue<Set<AString>> getInverse() {
		Set<AString> inverseSet = new HashSet<AString>();
		for (AString as : aStrings) {
			inverseSet.add(as.getInverse());
		}
		StringSetValue inverse = new StringSetValue();
		inverse.aStrings = inverseSet;
		return inverse;
	}

	@Override
	public StringSetValue getCopy() {
		return new StringSetValue(aStrings);
	}

	@Override
	public String toString() {
		return "StringSetValue [aStrings=" + aStrings + "]";
	}

}
