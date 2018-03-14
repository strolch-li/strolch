package li.strolch.search;

import java.util.Date;

import li.strolch.utils.ObjectHelper;
import li.strolch.utils.collections.DateRange;

public class PredicatesSupport {

	public static SearchPredicate isEqualTo(Object right) {
		return left -> ObjectHelper.equals(left, right, false);
	}

	public static SearchPredicate isEqualToIgnoreCase(Object right) {
		return left -> ObjectHelper.equals(left, right, true);
	}

	public static SearchPredicate isNotEqualTo(Object right) {
		return left -> !ObjectHelper.equals(left, right, false);
	}

	public static SearchPredicate isNotEqualToIgnoreCase(Object right) {
		return left -> !ObjectHelper.equals(left, right, true);
	}

	public static SearchPredicate startsWith(Object right) {
		return left -> ObjectHelper.startsWith(left, right, false);
	}

	public static SearchPredicate startsWithIgnoreCase(Object right) {
		return left -> ObjectHelper.startsWith(left, right, true);
	}

	public static SearchPredicate endsWith(Object right) {
		return left -> ObjectHelper.endsWith(left, right, false);
	}

	public static SearchPredicate endsWithIgnoreCase(Object right) {
		return left -> ObjectHelper.endsWith(left, right, true);
	}

	public static SearchPredicate contains(Object right) {
		return left -> ObjectHelper.contains(left, right, false);
	}

	public static SearchPredicate containsIgnoreCase(Object right) {
		return left -> ObjectHelper.contains(left, right, true);
	}

	public static SearchPredicate inRange(DateRange range) {
		return left -> range.contains((Date) left);
	}
}
