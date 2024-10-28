/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Boolean OR implementation to combine {@link ComparisonExpression} objects
 *
 * @author msmock
 */
public class OrExpression extends AbstractBooleanExpression {

	private final List<AndExpression> children = new ArrayList<>();

	/**
	 * @return true if at least one child expression returns true
	 */
	@Override
	public boolean evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		for (AndExpression andExpression : children) {
			if (!andExpression.evaluate(inputObjects, queryParameter))
				return false;
		}
		return true;
	}

	public void addAndExpression(final AndExpression andExpression) {
		children.add(andExpression);
		andExpression.setParent(this);
	}

	@Override
	public String toString() {
		return "OrExpression [children=" + children + "]";
	}

}
