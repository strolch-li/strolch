package li.strolch.utils;

import java.util.Collection;
import java.util.Objects;

public class ObjectHelper {

	public static boolean equals(Object left, Object right, boolean ignoreCase) {
		if (left == right)
			return true;

		if (left != null && right != null) {
			if (ignoreCase && left instanceof String && right instanceof String)
				return ((String) left).equalsIgnoreCase((String) right);

			boolean leftIsArray = left.getClass().isArray();
			boolean rightIsArray = right.getClass().isArray();
			return leftIsArray && rightIsArray ? Objects.deepEquals(left, right) : left.equals(right);
		}

		return false;
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
			Comparable comparable = (Comparable) left;

			@SuppressWarnings("unchecked")
			int i = comparable.compareTo(right);
			return i;
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

		if (left instanceof Collection) {
			Collection<?> collection = (Collection) left;
			if (right instanceof Collection)
				return collection.containsAll((Collection) right);
			return collection.contains(right);
		}

		if (left instanceof String) {
			String str = (String) left;

			if (right instanceof String[]) {
				String[] arr = (String[]) right;

				if (ignoreCase) {
					str = str.toLowerCase();
					for (String s : arr) {
						if (!str.contains(s.toLowerCase()))
							return false;
					}

					return true;

				} else {
					for (String s : arr) {
						if (!str.contains(s))
							return false;
					}

					return true;
				}
			}

			if (right instanceof String) {
				String subStr = (String) right;

				if (ignoreCase)
					return str.toLowerCase().contains(subStr.toLowerCase());
				else
					return str.contains(subStr);
			}
		}

		// comparing non-strings we use equals, as contains fits as well
		if (left.getClass() == right.getClass())
			return left.equals(right);

		throw new IllegalArgumentException("Unhandled type combination " + left.getClass() + " / " + right.getClass());
	}

	public static boolean isIn(Object left, Object right, boolean ignoreCase) {
		if (left == null && right == null)
			return true;
		if (left == null)
			return false;
		if (right == null)
			return false;

		if (right instanceof Collection) {
			if (left instanceof Collection) {
				Collection<?> collectionRight = (Collection) right;
				Collection<?> collectionleft = (Collection) left;
				for (Object oLeft : collectionleft) {
					for (Object oRight : collectionRight) {
						if (equals(oLeft, oRight, ignoreCase))
							return true;
					}
				}
				return false;
			} else {
				Collection<?> collection = (Collection) right;
				for (Object o : collection) {
					if (equals(left, o, ignoreCase))
						return true;
				}

				return false;
			}

		}

		if (right instanceof Object[]) {
			Object[] arr = (Object[]) right;
			for (Object o : arr) {
				if (equals(left, o, ignoreCase))
					return true;
			}

			return false;
		}

		if (right instanceof String || right instanceof Number) {
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

		if (left instanceof String && right instanceof String) {
			String str = (String) left;
			String subStr = (String) right;

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

		if (left instanceof String && right instanceof String) {
			String str = (String) left;
			String subStr = (String) right;

			if (ignoreCase)
				return str.toLowerCase().endsWith(subStr.toLowerCase());
			return str.endsWith(subStr);
		}

		throw new IllegalArgumentException("Unhandled type combination " + left.getClass() + " / " + right.getClass());
	}
}
