package li.strolch.model;

import java.util.List;

import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.builder.TemplatesBuilder;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TemplateBuilderTest {

	private static final Logger logger = LoggerFactory.getLogger(TemplateBuilderTest.class);

	@Test
	public void shouldBuildTemplates() {

		List<StrolchRootElement> templates = new TemplatesBuilder() //

				/*
				 * resource templates
				 */

				// person
				.resource("Person Template", "Person") //
				.defaultBag() //
				.string("birthdate", "Birthdate").end() //
				.string("case", "Case").interpretation("Case").uom("Simple").end() //
				.endBag() //
				.resourceRelation("car", "Personal Card", "Car") //
				.policies() //
				.planning("key:SimplePlanning") //
				.execution("key:DurationExecution") //
				.confirmation("key:NoConfirmation") //
				.endPolicies() //
				.endResource() //

				// cars
				.resource("Car Template", "Car") //
				.defaultBag() //
				.string("color", "Color").end() //
				.endBag() //
				.resourceRelation("Owner") //
				.endResource() //

				// machines
				.resource("Machine Template", "Machine") //
				.defaultBag() //
				.string("color", "Color").end() //
				.endBag() //
				.booleanState("driving", "Driving").end()//
				.resourceRelation("Owner") //
				.endResource() //

				/*
				 * order templates
				 */

				// orders
				.order("Order Template", "Order") //
				.defaultBag() //
				.string("description", "Description").end() //
				.endBag() //
				.bag("products", "Products", "Products") //
				.string("quantity", "Quantity").end() //
				.endBag() //
				.policies().policy("CostCalculationPolicy", "key:SimpleCostCalculation").endPolicies() //
				.endOrder() //

				/*
				 * activity templates
				 */

				// ToStock
				.activity("ToStock Template", "ToStock", TimeOrdering.SERIES) //
				.defaultBag() //
				.string("description", "Description").end() //
				.endBag() //
				.objectivesBag() //
				.string("quantity", "Quantity").end() //
				.endBag() //

				// level 1 action
				.action("lvl1Action", "Level 1 Action Element 1", "SubAction") //
				.resource("Machine", "machine1") //
				.objectivesBag() //
				.string("quantity", "Quantity").end() //
				.endBag() //
				.endAction() //

				// level 1 activity
				.subActivity("lvl1", "Level 1 Activity Element 2", "SubActivity", TimeOrdering.SERIES) //

				// level 2 action
				.action("lvl2Action", "Level 2 Action Element 1", "SubAction") //
				.resource("Machine", "machine1") //
				.objectivesBag() //
				.string("quantity", "Quantity").end() //
				.endBag() //
				.endAction() //

				// level 2 activity
				.subActivity("lvl2Activity", "Level 2 Activity Element 2", "SubActivity", TimeOrdering.SERIES) //

				// level 3 action
				.action("lvl3Action1", "Level 3 Action Element 1", "SubAction") //
				.resource("Machine", "machine2") //
				.objectivesBag() //
				.string("quantity", "Quantity").end() //
				.endBag() //
				.endAction() //

				.endSubActivity() // level 2 activity

				.endSubActivity() // level 1 activity

				.endActivity() // root

				//
				.buildTemplates();

		Assert.assertEquals(5, templates.size());
		templates.forEach(element -> logger.info("\n" + element.toXmlString()));
	}
}
