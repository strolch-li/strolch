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
 * Expression to wrap a declaration of method arguments
 *
 * @author msmock
 */
public class MethodArgumentDeclaration extends AbstractObjectExpression {

	private final List<ParameterReference> parameterReferences = new ArrayList<>();

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return List.class;
	}

	@Override
	public Object evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		final List<Object> result = new ArrayList<>();
		for (ParameterReference parameterReference : parameterReferences) {
			result.add(parameterReference.evaluate(inputObjects, queryParameter));
		}
		return result;
	}

	public void addParameterReference(final ParameterReference parameterReference) {
		parameterReferences.add(parameterReference);
		parameterReference.setParent(this);
	}

	@Override
	public String toString() {
		return "MethodArguments [parameterReferences=" + parameterReferences + "]";
	}

}
