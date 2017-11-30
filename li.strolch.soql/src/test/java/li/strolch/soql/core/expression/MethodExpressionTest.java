package li.strolch.soql.core.expression;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import li.strolch.soql.core.MockObject;
import li.strolch.soql.core.MockParameter;
import li.strolch.soql.core.expresssion.MethodArgumentDeclaration;
import li.strolch.soql.core.expresssion.MethodExpression;
import li.strolch.soql.core.expresssion.ParameterReference;

public class MethodExpressionTest {

	@Test
	public void test() {

		String matchingKey = "testString";

		MockObject mockObject = new MockObject();
		mockObject.putParameter(matchingKey, new MockParameter());

		MethodExpression methodExpression = new MethodExpression();
		methodExpression.setMethodName("getParameter");
		methodExpression.setObject(mockObject);

		ParameterReference parameterReference = new ParameterReference();
		parameterReference.varName = "param_1";

		MethodArgumentDeclaration argument = new MethodArgumentDeclaration();
		argument.addParameterReference(parameterReference);

		methodExpression.setMethodArguments(argument);

		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);

		Object result = methodExpression.evaluate(null, queryParameter);
		
		assertEquals(MockParameter.class, result.getClass());

	}

}
