/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

	TimeOrdering(String name) {
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
