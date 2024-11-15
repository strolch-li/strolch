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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * extract state field value by chained method call
 *
 * @author msmock
 */
public class ChainedMethodExpression extends AbstractObjectExpression {

	private String objectKey;

	private final List<MethodExpression> methodExpressions = new ArrayList<>();

	@Override
	public Class<?> getType(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		Object result = evaluate(inputObjects, queryParameter);
		return result.getClass();
	}

	/**
	 * evaluate by calling the method by name.
	 */
	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

		Object object = inputObjects.get(objectKey);
		for (MethodExpression methodExpression : methodExpressions) {
			methodExpression.setObject(object);
			object = methodExpression.evaluate(inputObjects, queryParameter);
			if (object == null)
				return null;
		}

		return object;
	}

	public void setObjectKey(String entityKey) {
		this.objectKey = entityKey;
	}

	public void addMethodExpression(MethodExpression methodExpression) {
		this.methodExpressions.add(methodExpression);
		methodExpression.setParent(this);
	}

	@Override
	public String toString() {
		return "ChainedMethodExpression [objectKey=" + objectKey + ", methodExpressions=" + methodExpressions + "]";
	}

}
