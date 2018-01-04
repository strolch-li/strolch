package li.strolch.soql.core;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.soql.core.expresssion.ChainedMethodExpression;
import li.strolch.soql.core.expresssion.MethodArgumentDeclaration;
import li.strolch.soql.core.expresssion.MethodExpression;
import li.strolch.soql.core.expresssion.ParameterReference;

/**
 * @author msmock
 */
public class MethodExpressionTest {

    public StrolchRootElement getTestElement() {
        final Resource resource = new Resource();
        resource.setId("testId");

        final ParameterBag bag = new ParameterBag();
        bag.setId("testBag");
        resource.addParameterBag(bag);

        final Parameter<String> parameter = new StringParameter();
        parameter.setId("testId");
        parameter.setValue("testValue");

        resource.addParameter("testBag", parameter);
        return resource;
    }

    @Test
    public void testMethod() {

        MethodExpression methodExpression = new MethodExpression();
        methodExpression.setMethodName("getParameter");
        methodExpression.setObject(getTestElement());

        MethodArgumentDeclaration argument = new MethodArgumentDeclaration();

        ParameterReference parameterReference = new ParameterReference();
        parameterReference.varName = "p_1";
        argument.addParameterReference(parameterReference);

        parameterReference = new ParameterReference();
        parameterReference.varName = "p_2";
        argument.addParameterReference(parameterReference);

        methodExpression.setMethodArguments(argument);

        Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "testId");

        Object result = methodExpression.evaluate(null, queryParameter);

        assertEquals(StringParameter.class, result.getClass());

    }

    @Test
    public void testChained() {

        MethodArgumentDeclaration argument = new MethodArgumentDeclaration();

        ParameterReference parameterReference = new ParameterReference();
        parameterReference.varName = "p_1";
        argument.addParameterReference(parameterReference);

        parameterReference = new ParameterReference();
        parameterReference.varName = "p_2";
        argument.addParameterReference(parameterReference);

        MethodExpression methodExpression_1 = new MethodExpression();
        methodExpression_1.setMethodName("getParameter");
        methodExpression_1.setMethodArguments(argument);

        MethodExpression methodExpression_2 = new MethodExpression();
        methodExpression_2.setMethodName("getType");
        methodExpression_2.setMethodArguments(new MethodArgumentDeclaration());

        // build the chained expression
        ChainedMethodExpression chainedMethodExpression = new ChainedMethodExpression();
        chainedMethodExpression.setObjectKey("a");
        chainedMethodExpression.addMethodExpression(methodExpression_1);
        chainedMethodExpression.addMethodExpression(methodExpression_2);

        // prepare the runtime objects

        Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("a", getTestElement());

        Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "testId");

        // evaluate the chained expression
        Object result = chainedMethodExpression.evaluate(inputObjects, queryParameter);

        assertEquals("String", result);
    }

}
