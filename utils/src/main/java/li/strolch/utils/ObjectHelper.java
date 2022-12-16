package li.strolch.utils;

import java.util.Collection;
import java.util.Date;
import java.util.Iterator;

import li.strolch.utils.iso8601.ISO8601;

public class ObjectHelper {

	public static boolean equals(Object left, Object right, boolean ignoreCase) {
		if (left == null && right == null)
			return true;
		if (left == right)
			return true;
		if (left == null || right == null)
			return false;

		if (left instanceof Collection<?> leftCollection) {

			if (right instanceof Collection<?> rightCollection) {
				if (leftCollection.size() != rightCollection.size())
					return false;

				Iterator<?> leftIter = leftCollection.iterator();
				Iterator<?> rightIter = rightCollection.iterator();

				while (leftIter.hasNext()) {
					Object l = leftIter.next();
					Object r = rightIter.next();

					// since we ignore case, we can toString()
					if (ignoreCase) {
						if (!l.toString().equalsIgnoreCase(r.toString()))
							return false;
					} else {
						if (!l.equals(r))
							return false;
					}
				}

				return true;
			}

			if (right instanceof String[] rightArr) {

				int i = 0;
				for (Object l : leftCollection) {
					Object r = rightArr[i];

					// since we ignore case, we can toString()
					if (ignoreCase) {
						if (!l.toString().equalsIgnoreCase(r.toString()))
							return false;
					} else {
						if (!l.equals(r))
							return false;
					}

					i++;
				}

				return true;
			}

			// since right is neither a collection nor an array, we can't check for equals!
			return false;
		}

		if (left instanceof String && right instanceof Enum<?>)
			right = ((Enum<?>) right).name();
		else if (right instanceof String && left instanceof Enum<?>)
			left = ((Enum<?>) left).name();

		if (left.getClass() != right.getClass())
			return false;

		// since we ignore case, we can toString()
		if (ignoreCase)
			return left.toString().equalsIgnoreCase(right.toString());

		return left.equals(right);
	}

	public static int compare(Object left, Object right, boolean ignoreCase) {
		if (left == right)
			return 0;
		if (left == null)
			return -1;
		if (right == null)
			return 1;

		if (ignoreCase && left instanceof String && right instanceof String)
			return ((String) left).compareToIgnoreCase((String) right);

		if (left instanceof Comparable) {
			@SuppressWarnings("unchecked")
			Comparable<Object> comparable = (Comparable<Object>) left;

			return comparable.compareTo(right);
		}

		int answer = left.getClass().getName().compareTo(right.getClass().getName());
		return (answer == 0) ? left.hashCode() - right.hashCode() : answer;
	}

	public static boolean contains(Object left, Object right, boolean ignoreCase) {
		if (left == null && right == null)
			return true;
		if (left == null)
			return false;
		if (right == null)
			return false;

		if (left instanceof Collection<?> leftCollection) {

			if (right instanceof Collection<?> rightCollection) {
				for (Object l : leftCollection) {
					for (Object r : rightCollection) {
						if (contains(l, r, ignoreCase))
							return true;
					}
				}

				return false;
			}

			if (right instanceof String[] rightArr) {
				for (Object l : leftCollection) {
					for (Object r : rightArr) {
						if (contains(l, r, ignoreCase))
							return true;
					}
				}

				return false;
			}

			for (Object l : leftCollection) {
				if (contains(l, right, ignoreCase))
					return true;
			}

			return false;
		}

		if (left instanceof String leftString) {

			if (right instanceof String[] rightArr) {

				if (ignoreCase) {
					leftString = leftString.toLowerCase();
					for (String s : rightArr) {
						if (!leftString.contains(s.toLowerCase()))
							return false;
					}

					return true;

				} else {
					for (String s : rightArr) {
						if (!leftString.contains(s))
							return false;
					}

					return true;
				}
			}

			if (right.getClass().isEnum())
				right = ((Enum<?>) right).name();

			if (right instanceof String rightString) {

				if (ignoreCase)
					return leftString.toLowerCase().contains(rightString.toLowerCase());
				else
					return leftString.contains(rightString);
			}
		}

		// comparing non-strings we use equals, as contains fits as well
		if (left.getClass() == right.getClass())
			return left.equals(right);

		// try to coerce the right side to the left side
		if (right instanceof String rightString) {
			Object rightO;
			if (left instanceof Integer) {
				rightO = Integer.valueOf(rightString);
			} else if (left instanceof Float) {
				rightO = Float.valueOf(rightString);
			} else if (left instanceof Double) {
				rightO = Double.valueOf(rightString);
			} else if (left instanceof Boolean) {
				rightO = Boolean.valueOf(rightString);
			} else if (left instanceof Date) {
				rightO = ISO8601.parseToDate(rightString);
			} else {
				throw new IllegalArgumentException(
						"Unhandled type combination " + left.getClass() + " / " + right.getClass());
			}

			return rightO.equals(left);
		}

		throw new IllegalArgumentException("Unhandled type combination " + left.getClass() + " / " + right.getClass());
	}

	public static boolean isIn(Object left, Object right, boolean ignoreCase) {
		if (left == null && right == null)
			return true;
		if (left == null)
			return false;
		if (right == null)
			return false;

		if (right instanceof Collection<?> collectionRight) {

			if (left instanceof Collection<?> collectionLeft) {
				for (Object l : collectionLeft) {
					for (Object r : collectionRight) {
						if (equals(r, l, ignoreCase))
							return true;
					}
				}

				return false;

			} else if (left instanceof String[] leftArr) {
				for (Object r : collectionRight) {
					for (Object l : leftArr) {
						if (equals(r, l, ignoreCase))
							return true;
					}
				}

				return false;

			} else {
				for (Object o : collectionRight) {
					if (equals(o, left, ignoreCase))
						return true;
				}

				return false;
			}
		}

		if (right instanceof Object[] arr) {
			for (Object o : arr) {
				if (equals(left, o, ignoreCase))
					return true;
			}

			return false;
		}

		if (right instanceof String || right instanceof Number || right instanceof Boolean) {
			return equals(left, right, ignoreCase);
		}

		throw new IllegalArgumentException("Unhandled type combination " + left.getClass() + " / " + right.getClass());
	}

	public static boolean startsWith(Object left, Object right, boolean ignoreCase) {
		if (left == null && right == null)
			return true;
		if (left == null)
			return false;
		if (right == null)
			return false;

		if (left instanceof String str && right instanceof String subStr) {

			if (ignoreCase)
				return str.toLowerCase().startsWith(subStr.toLowerCase());
			return str.startsWith(subStr);
		}

		throw new IllegalArgumentException("Unhandled type combination " + left.getClass() + " / " + right.getClass());
	}

	public static boolean endsWith(Object left, Object right, boolean ignoreCase) {
		if (left == null && right == null)
			return true;
		if (left == null)
			return false;
		if (right == null)
			return false;

		if (left instanceof String str && right instanceof String subStr) {

			if (ignoreCase)
				return str.toLowerCase().endsWith(subStr.toLowerCase());
			return str.endsWith(subStr);
		}

		throw new IllegalArgumentException("Unhandled type combination " + left.getClass() + " / " + right.getClass());
	}

	public static boolean isEmpty(Object object) {
		if (object == null)
			return true;
		if (object instanceof String)
			return ((String) object).isEmpty();
		if (object instanceof Boolean)
			return !((Boolean) object);
		if (object instanceof Number)
			return ((Number) object).doubleValue() == 0.0D;

		return false;
	}
}
