package li.strolch.model.activity;

import li.strolch.exception.StrolchException;
import li.strolch.utils.dbc.DBC;

public enum TimeOrdering {
	SERIES("Series") {
		@Override
		public void accept(TimeOrderingVisitor visitor, Activity activity) {
			visitor.visitSeries(activity);
		}
	},
	PARALLEL("Parallel") {
		@Override
		public void accept(TimeOrderingVisitor visitor, Activity activity) {
			visitor.visitParallel(activity);
		}
	};

	private final String name;

	private TimeOrdering(String name) {
		this.name = name;
	}

	public String getName() {
		return this.name;
	}

	public abstract void accept(TimeOrderingVisitor visitor, Activity activity);

	public static TimeOrdering parse(String s) {
		DBC.PRE.assertNotEmpty("Value may not be null", s);
		for (TimeOrdering timeOrdering : values()) {
			if (timeOrdering.name.equals(s))
				return timeOrdering;
		}

		throw new StrolchException("No TimeOrdering for " + s);
	}
}
