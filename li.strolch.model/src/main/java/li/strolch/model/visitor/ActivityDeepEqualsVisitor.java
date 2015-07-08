/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.visitor;

import java.util.List;

import li.strolch.model.ActivityVisitor;
import li.strolch.model.Locator;
import li.strolch.model.activity.Activity;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ActivityDeepEqualsVisitor extends StrolchElementDeepEqualsVisitor implements
		ActivityVisitor<List<Locator>> {

	private Activity sourceActivity;

	public ActivityDeepEqualsVisitor(Activity sourceActivity) {
		this.sourceActivity = sourceActivity;
	}

	@Override
	public List<Locator> visit(Activity dstActivity) {
		deepEquals(this.sourceActivity, dstActivity);
		return getMismatchedLocators();
	}
}
