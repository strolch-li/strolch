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
			else
				return collection.contains(right);
		}

		if (left instanceof String && right instanceof String) {
			String str = (String) left;
			String subStr = (String) right;

			if (ignoreCase)
				return str.toLowerCase().contains(subStr.toLowerCase());
			else
				return str.contains(subStr);
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
			else
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
			else
				return str.endsWith(subStr);
		}

		throw new IllegalArgumentException("Unhandled type combination " + left.getClass() + " / " + right.getClass());
	}
}
