package li.strolch.model;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.BAG_OBJECTIVES;
import static li.strolch.model.StrolchModelConstants.TEMPLATE;
import static org.junit.Assert.*;

import java.time.ZonedDateTime;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.builder.StrolchElementBuilder;
import li.strolch.utils.collections.MapOfMaps;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StrolchElementBuilderTest {

	private static final Logger logger = LoggerFactory.getLogger(StrolchElementBuilderTest.class);

	@Test
	public void shouldBuildTemplates() {

		StrolchElementBuilder builder = new StrolchElementBuilder() //

				/*
				 * resource templates
				 */

				// person
				.resource("Person Template", "Person") //
				.defaultBag() //
				.date("birthdate", "Birthdate").value(ZonedDateTime.now()).end() //
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
				.string("color", "Color").value("white").end() //
				.endBag() //
				.resourceRelation("Owner") //
				.booleanState("driving", "Driving").end()//
				.endResource() //

				// machines
				.resource("Machine Template", "Machine") //
				.defaultBag() //
				.string("color", "Color").end() //
				.endBag() //
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
				.integer("quantity", "Quantity").interpretation("Volume").uom("mm3").end() //
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
				.integer("quantity", "Quantity").end() //
				.endBag() //

				// level 1 action
				.action("lvl1ActionElem1", "Level 1 Action Element 1", "SubAction") //
				.resource("Machine", "machine1") //
				.objectivesBag() //
				.integer("quantity", "Quantity").end() //
				.endBag() //
				.endAction() //

				// level 1 activity
				.subActivity("lvl1Elem2", "Level 1 Activity Element 2", "SubActivity", TimeOrdering.SERIES) //

				// level 2 action
				.action("lvl2ActionElem1", "Level 2 Action Element 1", "SubAction") //
				.resource("Machine", "machine2") //
				.objectivesBag() //
				.integer("quantity", "Quantity").end() //
				.endBag() //
				.endAction() //

				// level 2 activity
				.subActivity("lvl2ActivityElem2", "Level 2 Activity Element 2", "SubActivity", TimeOrdering.SERIES) //

				// level 3 action
				.action("lvl3Action1Elem1", "Level 3 Action Element 1", "SubAction") //
				.resource("Machine", "machine3") //
				.objectivesBag() //
				.integer("quantity", "Quantity").end() //
				.endBag() //
				.endAction() //

				.endSubActivity() // level 2 activity

				.endSubActivity() // level 1 activity

				.policies() //
				.activityArchival("key:DefaultActivityArchival") //
				.endPolicies() //

				.endActivity() // root

				;

		List<StrolchRootElement> templates = builder.buildTemplates();
		MapOfMaps<String, String, StrolchRootElement> elementsByType = templates.stream().collect(MapOfMaps::new,
				(map, element) -> map.addElement(element.getObjectType(), element.getId(), element), MapOfMaps::putAll);
		assertEquals(5, elementsByType.size());
		elementsByType.forEach((t, map) -> map.values().forEach(element -> logger.info("\n" + element.toXmlString())));

		Resource carT = (Resource) elementsByType.getElement(Tags.RESOURCE, "Car");
		assertEquals(TEMPLATE, carT.getType());
		assertEquals("Car", carT.getId());
		assertEquals("white", carT.getString("color"));
		assertTrue(carT.hasRelation("owner"));
		assertTrue(carT.hasTimedState("driving"));
		assertEquals(StrolchValueType.BOOLEAN, carT.getTimedState("driving").getValueType());

		Order orderT = (Order) elementsByType.getElement(Tags.ORDER, "Order");
		assertEquals(TEMPLATE, orderT.getType());
		assertEquals("Order", orderT.getId());
		assertEquals("", orderT.getString("description"));
		assertTrue(orderT.hasParameterBag("products"));
		assertEquals(0, orderT.getInteger("products", "quantity"), 0);
		assertEquals("Volume", orderT.getIntegerP("products", "quantity").getInterpretation());
		assertEquals("mm3", orderT.getIntegerP("products", "quantity").getUom());
		assertEquals("key:SimpleCostCalculation", orderT.getPolicyDef("CostCalculationPolicy").getValueForXml());

		Activity activityT = (Activity) elementsByType.getElement(Tags.ACTIVITY, "ToStock");
		assertEquals(TEMPLATE, activityT.getType());
		assertEquals("ToStock", activityT.getId());
		assertEquals(TimeOrdering.SERIES, activityT.getTimeOrdering());
		assertTrue(activityT.hasParameterBag(BAG_OBJECTIVES));
		assertEquals(0, activityT.getInteger(BAG_OBJECTIVES, "quantity"), 0);

		Map<String, IActivityElement> elements = activityT.getElements();
		Iterator<IActivityElement> lvl1Iter = elements.values().iterator();

		IActivityElement lvl1Elem1 = lvl1Iter.next();
		assertEquals("lvl1ActionElem1", lvl1Elem1.getId());
		assertEquals(Action.class, lvl1Elem1.getClass());
		assertEquals("machine1", ((Action) lvl1Elem1).getResourceId());
		assertEquals("Machine", ((Action) lvl1Elem1).getResourceType());

		IActivityElement lvl1Elem2 = lvl1Iter.next();
		assertEquals("lvl1Elem2", lvl1Elem2.getId());
		assertEquals(Activity.class, lvl1Elem2.getClass());

		assertFalse(lvl1Iter.hasNext());

		Iterator<IActivityElement> lvl2Iter = ((Activity) lvl1Elem2).getElements().values().iterator();

		IActivityElement lvl2Elem1 = lvl2Iter.next();
		assertEquals("lvl2ActionElem1", lvl2Elem1.getId());
		assertEquals(Action.class, lvl2Elem1.getClass());
		assertEquals("machine2", ((Action) lvl2Elem1).getResourceId());
		assertEquals("Machine", ((Action) lvl2Elem1).getResourceType());

		IActivityElement lvl2Elem2 = lvl2Iter.next();
		assertEquals("lvl2ActivityElem2", lvl2Elem2.getId());
		assertEquals(Activity.class, lvl2Elem2.getClass());

		assertFalse(lvl2Iter.hasNext());

		Iterator<IActivityElement> lvl3Iter = ((Activity) lvl2Elem2).getElements().values().iterator();

		IActivityElement lvl3Elem1 = lvl3Iter.next();
		assertEquals("lvl3Action1Elem1", lvl3Elem1.getId());
		assertEquals(Action.class, lvl3Elem1.getClass());
		assertEquals("machine3", ((Action) lvl3Elem1).getResourceId());
		assertEquals("Machine", ((Action) lvl3Elem1).getResourceType());

		assertFalse(lvl3Iter.hasNext());

		assertEquals("key:DefaultActivityArchival", activityT.getPolicyDef("ActivityArchivalPolicy").getValueForXml());
	}

	@Test
	public void shouldBuildElements() {

		Resource car1 = new StrolchElementBuilder() //

				.resource("Car Template", "Car") //
				.defaultBag() //
				.string("color", "Color").value("white").end() //
				.endBag() //
				.resourceRelation("Owner") //
				.endResource() //

				.newResource("Car", "My Car");

		car1.setString("color", "green");
		car1.setRelationId("owner", "me!");
		logger.info("\n" + car1.toXmlString());
	}
}
