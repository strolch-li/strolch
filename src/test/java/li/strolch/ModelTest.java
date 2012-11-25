package li.strolch;

import li.strolch.model.Order;
import li.strolch.model.Parameter;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.parameter.FloatParameter;

import org.junit.Test;

public class ModelTest {

	@Test
	public void shouldCreateResource() {

		Resource resource = new Resource("@res01", "Test resource", "MyType");

		ParameterBag bag = new ParameterBag("@bag01", "Test Bag", "Test");

		Parameter<Double> floatParam = new FloatParameter("@param1", "Float Param", 44.3);
		bag.addParameter(floatParam);

		resource.addParameterBag(bag);
	}

	@Test
	public void shouldCreateOrder() {

		Order order = new Order("@ord01", "Test Order", "MyType", System.currentTimeMillis(), State.OPEN);

		ParameterBag bag = new ParameterBag("@bag01", "Test Bag", "Test");

		Parameter<Double> floatParam = new FloatParameter("@param1", "Float Param", 44.3);
		bag.addParameter(floatParam);

		order.addParameterBag(bag);
	}
}
