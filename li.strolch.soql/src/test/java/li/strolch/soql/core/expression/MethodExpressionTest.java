package li.strolch.soql.core.expression;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import org.junit.Test;

import li.strolch.soql.core.expresssion.MethodArgumentDeclaration;
import li.strolch.soql.core.expresssion.MethodExpression;
import li.strolch.soql.core.expresssion.ParameterReference;

public class MethodExpressionTest {

	/**
	 * @return a test parameter with String value
	 */
	public StrolchElement getTestElement() {
		final Resource resource = new Resource();
		resource.setId("testId");

		final ParameterBag bag = new ParameterBag();
		bag.setId("testBag");
		resource.addParameterBag(bag);

		final Parameter parameter = new StringParameter();
		parameter.setId("testId");
		parameter.setValue("testValue");

		resource.addParameter("testBag", parameter);
		return resource;
	}

	@Test
	public void test() {

		String matchingKey = "testString";

		MethodExpression methodExpression = new MethodExpression();
		methodExpression.setMethodName("getParameter");
		methodExpression.setObject(getTestElement());

		ParameterReference parameterReference = new ParameterReference();
		parameterReference.varName = "param_1";

		MethodArgumentDeclaration argument = new MethodArgumentDeclaration();
		argument.addParameterReference(parameterReference);

		methodExpression.setMethodArguments(argument);

		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);

		Object result = methodExpression.evaluate(null, queryParameter);
		
		assertEquals(StringParameter.class, result.getClass());

	}

}
