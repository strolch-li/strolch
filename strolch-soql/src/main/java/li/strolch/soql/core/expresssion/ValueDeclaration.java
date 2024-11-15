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

package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * Expression to set a fixed value
 * <p>
 * TODO allow blancs in Strings, and support parsing to other objects like int, double, Dates, etc.
 *
 * @author msmock
 */
public class ValueDeclaration extends AbstractObjectExpression {

	public String valueAsString;

	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		return valueAsString;
	}

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return String.class;
	}

	@Override
	public String toString() {
		return "ValueDeclaration [valueAsString=" + valueAsString + "]";
	}
}
