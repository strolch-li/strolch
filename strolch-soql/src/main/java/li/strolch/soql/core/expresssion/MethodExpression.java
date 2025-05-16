/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.soql.core.SOQLEvaluationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * extract state field value by reflection
 *
 * @author msmock
 */
public class MethodExpression extends AbstractObjectExpression {

	private static final Logger logger = LoggerFactory.getLogger(MethodExpression.class);

	private Object object;
	private String methodName;
	private MethodArgumentDeclaration methodArguments;

	@Override
	public Class<?> getType(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		Object result = evaluate(inputObjects, queryParameter);
		return result.getClass();
	}

	/**
	 * evaluate by calling the method by name.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

		// analyze the method arguments declaration
		List<Object> arguments = (List<Object>) methodArguments.evaluate(inputObjects, queryParameter);
		List<Class<?>> clazzes = new ArrayList<>();
		for (Object argument : arguments) {
			clazzes.add(argument.getClass());
		}

		// now find the method to call
		Method method;
		try {
			method = object.getClass().getMethod(methodName, clazzes.toArray(new Class<?>[0]));
		} catch (NoSuchMethodException e) {
			throw new SOQLEvaluationException("Method "
					+ methodName
					+ " with arguments "
					+ clazzes
					+ " not declared on object "
					+ object
					+ " of class "
					+ object.getClass());
		} catch (SecurityException e) {
			throw new RuntimeException("Failed to getMethod() " + this.methodName, e);
		}

		Object result;

		// and finally call the method
		try {
			result = method.invoke(object, arguments.toArray());
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			logger.error("Failed to call method {} on class {}", this.methodName, object.getClass().getName(), e);
			return null;
		}

		return result;
	}

	public void setObject(Object object) {
		this.object = object;
	}

	public void setMethodName(String methodName) {
		this.methodName = methodName;
	}

	public void setMethodArguments(MethodArgumentDeclaration methodArguments) {
		this.methodArguments = methodArguments;
		methodArguments.setParent(this);
	}

	@Override
	public String toString() {
		return "MethodExpression [object="
				+ object
				+ ", methodName="
				+ methodName
				+ ", methodArguments="
				+ methodArguments
				+ "]";
	}

}
