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
package li.strolch.model.query;

import li.strolch.model.ActivityVisitor;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.query.ordering.StrolchQueryOrdering;
import li.strolch.model.visitor.NoStrategyActivityVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * <p>
 * {@link ActivityQuery} is the user API to query {@link Activity Activities} in Strolch. The {@link Navigation} is used
 * to navigate to a type of activity on which any further {@link Selection Selections} will be performed. The
 * {@link ActivityVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query e.g. a specific {@link Action} instead of having to return all the elements and then
 * performing this transformation.
 * </p>
 * 
 * <p>
 * The {@link ActivityVisitor} is intended for situations where the query result should not be {@link Activity} but some
 * other object type. For instance in a restful API, the result might have to be mapped to a POJO, thus using this
 * method can perform the mapping step for you
 * </p>
 * 
 * @param <U>
 *            defines the return type of this query. Depending on the user {@link ActivityVisitor} this query can return
 *            an {@link Activity}, or any type of object to which the visitor mapped the activity
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ActivityQuery<U> extends StrolchElementQuery<ActivityQueryVisitor> {

	protected ActivityVisitor<U> activityVisitor;
	protected StrolchQueryOrdering ordering;

	public ActivityQuery() {
		super();
	}

	public ActivityQuery(Navigation navigation) {
		super(navigation);
	}

	public ActivityVisitor<U> getActivityVisitor() {
		return this.activityVisitor;
	}

	public ActivityQuery<U> setActivityVisitor(ActivityVisitor<U> activityVisitor) {
		DBC.PRE.assertNotNull("activityVisitor", activityVisitor);
		this.activityVisitor = activityVisitor;
		return this;
	}

	public StrolchQueryOrdering getOrdering() {
		return this.ordering;
	}

	public ActivityQuery<U> setOrdering(StrolchQueryOrdering ordering) {
		this.ordering = ordering;
		return this;
	}

	@Override
	public ActivityQuery<U> with(Selection selection) {
		super.with(selection);
		return this;
	}

	@Override
	public ActivityQuery<U> not(Selection selection) {
		super.not(selection);
		return this;
	}

	@Override
	public ActivityQuery<U> withAny() {
		super.withAny();
		return this;
	}

	@Override
	public void accept(ActivityQueryVisitor visitor) {
		super.accept(visitor);
		if (this.ordering != null)
			this.ordering.accept(visitor);
	}

	public static ActivityQuery<Activity> query(String type) {
		return new ActivityQuery<Activity>(new StrolchTypeNavigation(type))
				.setActivityVisitor(new NoStrategyActivityVisitor());
	}

	public static ActivityQuery<Activity> query(String type, StrolchQueryOrdering ordering) {
		return new ActivityQuery<Activity>(new StrolchTypeNavigation(type))
				.setActivityVisitor(new NoStrategyActivityVisitor()).setOrdering(ordering);
	}

	public static <U> ActivityQuery<U> query(String type, ActivityVisitor<U> activityVisitor) {
		return new ActivityQuery<U>(new StrolchTypeNavigation(type)).setActivityVisitor(activityVisitor);
	}

	public static <U> ActivityQuery<U> query(String type, ActivityVisitor<U> activityVisitor,
			StrolchQueryOrdering ordering) {
		return new ActivityQuery<U>(new StrolchTypeNavigation(type)).setActivityVisitor(activityVisitor)
				.setOrdering(ordering);
	}
}
