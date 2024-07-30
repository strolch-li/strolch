package li.strolch.model;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.xml.StrolchXmlHelper;

public class StrolchXmlHelperTest {

	@Test
	public void shouldWriteAndReadXml() {

		File outFile = new File("target/" + getClass().getSimpleName() + ".xml");

		Activity activity = ModelGenerator.createActivity("activity", "Activity", "Activity", TimeOrdering.SERIES);
		Resource resource = ModelGenerator.createResource("res", "Res", "Res");
		Order order = ModelGenerator.createOrder("order", "Order", "Order");

		Set<StrolchRootElement> elements = new HashSet<>(Arrays.asList(activity, resource, order));

		StrolchXmlHelper.writeToFile(outFile, elements);
		Set<StrolchRootElement> result = new HashSet<>(StrolchXmlHelper.parseFile(outFile));

		assertEquals(elements, result);
	}
}
