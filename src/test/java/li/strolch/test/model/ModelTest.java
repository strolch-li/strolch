package li.strolch.test.model;

import java.util.ArrayList;

import junit.framework.Assert;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

import org.junit.Test;

public class ModelTest {

	@Test
	public void shouldCreateResource() {

		Resource resource = ModelTestHelper.createResource("@res01", "Test resource", "MyType");
		ParameterBag bag = resource.getParameterBag(ModelTestHelper.BAG_ID);
		validateBag(bag);
	}

	@Test
	public void shouldCreateOrder() {

		Order order = ModelTestHelper.createOrder("@ord01", "Test Order", "MyType", System.currentTimeMillis(),
				State.OPEN);
		ParameterBag bag = order.getParameterBag(ModelTestHelper.BAG_ID);
		validateBag(bag);
	}

	public static void validateBag(ParameterBag bag) {

		Assert.assertNotNull(bag);

		Assert.assertEquals(ModelTestHelper.BAG_ID, bag.getId());
		Assert.assertEquals(ModelTestHelper.BAG_NAME, bag.getName());
		Assert.assertEquals(ModelTestHelper.BAG_TYPE, bag.getType());

		validateParams(bag);
	}

	public static void validateParams(ParameterBag bag) {

		BooleanParameter boolParam = bag.getParameter(ModelTestHelper.PARAM_BOOLEAN_ID);
		Assert.assertNotNull("Boolean Param missing with id " + ModelTestHelper.PARAM_BOOLEAN_ID, boolParam);
		Assert.assertEquals(true, boolParam.getValue().booleanValue());

		FloatParameter floatParam = bag.getParameter(ModelTestHelper.PARAM_FLOAT_ID);
		Assert.assertNotNull("Float Param missing with id " + ModelTestHelper.PARAM_FLOAT_ID, floatParam);
		Assert.assertEquals(44.3, floatParam.getValue().doubleValue());

		IntegerParameter integerParam = bag.getParameter(ModelTestHelper.PARAM_INTEGER_ID);
		Assert.assertNotNull("Integer Param missing with id " + ModelTestHelper.PARAM_INTEGER_ID, integerParam);
		Assert.assertEquals(77, integerParam.getValue().intValue());

		LongParameter longParam = bag.getParameter(ModelTestHelper.PARAM_LONG_ID);
		Assert.assertNotNull("Long Param missing with id " + ModelTestHelper.PARAM_LONG_ID, longParam);
		Assert.assertEquals(4453234566L, longParam.getValue().longValue());

		StringParameter stringParam = bag.getParameter(ModelTestHelper.PARAM_STRING_ID);
		Assert.assertNotNull("String Param missing with id " + ModelTestHelper.PARAM_STRING_ID, stringParam);
		Assert.assertEquals("Strolch", stringParam.getValue());

		DateParameter dateParam = bag.getParameter(ModelTestHelper.PARAM_DATE_ID);
		Assert.assertNotNull("Date Param missing with id " + ModelTestHelper.PARAM_DATE_ID, dateParam);
		Assert.assertEquals(1354295525628L, dateParam.getValue().longValue());

		StringListParameter stringListP = bag.getParameter(ModelTestHelper.PARAM_LIST_STRING_ID);
		Assert.assertNotNull("StringList Param missing with id " + ModelTestHelper.PARAM_LIST_STRING_ID, stringListP);

		ArrayList<String> stringList = new ArrayList<String>();
		stringList.add("Hello");
		stringList.add("World");
		Assert.assertEquals(stringList, stringListP.getValue());
	}
}
