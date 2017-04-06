/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.runtime.query.inmemory;

import java.util.List;

import li.strolch.model.activity.Activity;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.ActivityQueryVisitor;
import li.strolch.model.query.ActivityStateSelection;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.model.visitor.ActivityVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryActivityQueryVisitor extends InMemoryQueryVisitor<Activity> implements ActivityQueryVisitor {

	public InMemoryActivityQueryVisitor() {
		super();
	}

	@Override
	protected InMemoryQueryVisitor<Activity> newInstance() {
		return new InMemoryActivityQueryVisitor();
	}

	public <U> InMemoryQuery<Activity, U> visit(ActivityQuery<U> activityQuery) {
		ActivityVisitor<U> activityVisitor = activityQuery.getActivityVisitor();
		DBC.PRE.assertNotNull("ActivityVisitor may not be null!", activityVisitor); //$NON-NLS-1$

		activityQuery.accept(this);

		Navigator<Activity> navigator = getNavigator();
		if (navigator == null) {
			String msg = "Query is missing a navigation!"; //$NON-NLS-1$
			throw new QueryException(msg);
		}

		List<Selector<Activity>> selectors = getSelectors();
		if (selectors.isEmpty())
			return new InMemoryQuery<>(navigator, null, activityVisitor, getComparator());

		DBC.PRE.assertTrue("Invalid query as it may only contain one selector!", selectors.size() == 1); //$NON-NLS-1$
		return new InMemoryQuery<>(navigator, selectors.get(0), activityVisitor, getComparator());
	}

	@Override
	public void visit(StrolchTypeNavigation navigation) {
		setNavigator(new ActivityTypeNavigator(navigation.getType()));
	}

	@Override
	public void visit(ActivityStateSelection selection) {
		addSelector(new ActivityStateSelector(selection.getState()));
	}
}
