package li.strolch.model;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import li.strolch.model.activity.Activity;

public abstract class ModelMarshallingTest {

	@Test
	public void shouldFormatAndParseOrder() throws Exception {
		Order order = ModelGenerator.createOrder("@1", "My Order 1", "MyOrder");
		formatAndParseOrder(order);
	}

	@Test
	public void shouldFormatAndParseVersionedOrder() throws Exception {
		Order order = ModelGenerator.createOrder("@1", "My Order 1", "MyOrder");
		Version.setInitialVersionFor(order, "test");
		Order parsed = formatAndParseOrder(order);
		assertEquals(order.getVersion(), parsed.getVersion());
	}

	@Test
	public void shouldFormatAndParseResource() throws Exception {
		Resource resource = ModelGenerator.createResource("@1", "My Resource 1", "MyResource");
		formatAndParseResource(resource);
	}

	@Test
	public void shouldFormatAndParseVersionedResource() throws Exception {
		Resource resource = ModelGenerator.createResource("@1", "My Resource 1", "MyResource");
		Version.setInitialVersionFor(resource, "test");
		Resource parsed = formatAndParseResource(resource);
		assertEquals(resource.getVersion(), parsed.getVersion());
	}

	@Test
	public void shouldFormatAndParseActivity() throws Exception {
		Activity activity = ModelGenerator.createActivity("@1", "My Activity 1", "Transport");
		formatAndParseActivity(activity);
	}

	@Test
	public void shouldFormatAndParseVersionedActivity() throws Exception {
		Activity activity = ModelGenerator.createActivity("@1", "My Activity 1", "Transport");
		Version.setInitialVersionFor(activity, "test");
		Activity parsed = formatAndParseActivity(activity);
		assertEquals(activity.getVersion(), parsed.getVersion());
	}

	protected abstract Order formatAndParseOrder(Order order) throws Exception;

	protected abstract Resource formatAndParseResource(Resource resource) throws Exception;

	protected abstract Activity formatAndParseActivity(Activity activity) throws Exception;

}
