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

/**
 * {@link ActivityQuery} is the user API to query {@link Activity Activities} in Strolch. The {@link Navigation} is used
 * to navigate to a type of activity on which any further {@link Selection Selections} will be performed. The
 * {@link ActivityVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query e.g. a specific {@link Action} instead of having to return all the elements and then
 * performing this transformation.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ActivityQuery extends StrolchElementQuery<ActivityQueryVisitor> {

	/**
	 * @param navigation
	 * @param elementVisitor
	 */
	public ActivityQuery(Navigation navigation) {
		super(navigation);
	}

	@Override
	public ActivityQuery with(Selection selection) {
		super.with(selection);
		return this;
	}

	@Override
	public ActivityQuery not(Selection selection) {
		super.not(selection);
		return this;
	}

	@Override
	public ActivityQuery withAny() {
		super.withAny();
		return this;
	}

	public static ActivityQuery query(Navigation navigation) {
		return new ActivityQuery(navigation);
	}

	public static ActivityQuery query(String type) {
		return new ActivityQuery(new StrolchTypeNavigation(type));
	}
}
