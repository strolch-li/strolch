package li.strolch.search;

import java.util.Date;

import li.strolch.utils.ObjectHelper;
import li.strolch.utils.collections.DateRange;

public class SearchPredicates {

	public static SearchPredicate isEqualTo(Object right, boolean ignoreCase) {
		return left -> ObjectHelper.equals(left, right, ignoreCase);
	}

	public static SearchPredicate startsWith(Object right, boolean ignoreCase) {
		return left -> ObjectHelper.startsWith(left, right, ignoreCase);
	}

	public static SearchPredicate endsWith(Object right, boolean ignoreCase) {
		return left -> ObjectHelper.endsWith(left, right, ignoreCase);
	}

	public static SearchPredicate contains(Object right, boolean ignoreCase) {
		return left -> ObjectHelper.contains(left, right, ignoreCase);
	}

	public static SearchPredicate inRange(DateRange range) {
		return left -> range.contains((Date) left);
	}
}
