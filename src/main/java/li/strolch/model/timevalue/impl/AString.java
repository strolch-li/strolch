package li.strolch.model.timevalue.impl;

import java.io.Serializable;

/**
 * Wrapper for java.util.String object defining a inverse to support algebraic
 * operations.
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class AString implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private final String string;
	private final boolean inverse;

	public AString(final String string) {
		this.string = string;
		this.inverse = false;
	}

	public AString(final String string, final boolean inverse) {
		this.string = string;
		this.inverse = inverse;
	}

	public String getString() {
		return string;
	}

	public boolean isInverse() {
		return inverse;
	}

	public AString getInverse() {
		return new AString(string, !inverse);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (inverse ? 1231 : 1237);
		result = prime * result + ((string == null) ? 0 : string.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AString other = (AString) obj;
		if (inverse != other.inverse)
			return false;
		if (string == null) {
			if (other.string != null)
				return false;
		} else if (!string.equals(other.string))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "AString [string=" + string + ", inverse=" + inverse + "]";
	}

}
