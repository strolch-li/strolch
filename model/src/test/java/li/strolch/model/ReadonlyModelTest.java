package li.strolch.model;

import static li.strolch.model.ModelGenerator.*;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.*;

import java.util.Collections;
import java.util.Date;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.*;
import li.strolch.model.timedstate.*;
import li.strolch.model.timevalue.impl.*;
import org.hamcrest.MatcherAssert;
import org.junit.Test;

public class ReadonlyModelTest {

	@Test
	public void resourceShouldBeReadonly() {
		Resource resource = createResource("@res01", "Test resource", "MyType");
		resource.setReadOnly();
		assertTrue("Should be readOnly", resource.isReadOnly());
		assertElement(resource);
		assertParams(resource);
		assertStates(resource);
	}

	private void assertElement(StrolchElement element) {
		try {
			element.setId("sdf");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			element.setName("sdf");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
	}

	@Test
	public void orderShouldBeReadonly() {
		Order order = createOrder("@ord01", "Test Order", "MyType");
		order.setReadOnly();
		assertTrue("Should be readOnly", order.isReadOnly());
		assertElement(order);
		assertParams(order);

		try {
			order.setState(State.CREATED);
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
	}

	@Test
	public void activityShouldBeReadonly() {
		Activity activity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		activity.setReadOnly();
		assertTrue("Should be readOnly", activity.isReadOnly());
		assertElement(activity);
		assertParams(activity);

		try {
			activity.addElement(createAction("action_" + activity.getId(), "Action " + activity.getName(), "Use"));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		Action act = activity.getElement("action_" + activity.getId());
		assertElement(act);
		try {
			act.setState(State.EXECUTION);
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			act.setResourceId("dfg");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			act.addChange(new ValueChange<>(1L, new FloatValue(1.0D), "sdf"));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
	}

	private void assertParams(ParameterBagContainer bagContainer) {
		BooleanParameter boolP = bagContainer.getParameter(BAG_ID, PARAM_BOOLEAN_ID, true);
		assertElement(boolP);
		assertTrue("Should be readOnly", boolP.isReadOnly());
		try {
			boolP.setValue(false);
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		StringParameter stringP = bagContainer.getParameter(BAG_ID, PARAM_STRING_ID, true);
		assertTrue("Should be readOnly", stringP.isReadOnly());
		try {
			stringP.setValue("sdf");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		IntegerParameter integerP = bagContainer.getParameter(BAG_ID, PARAM_INTEGER_ID, true);
		assertTrue("Should be readOnly", integerP.isReadOnly());
		try {
			integerP.setValue(1);
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		DateParameter dateP = bagContainer.getParameter(BAG_ID, PARAM_DATE_ID, true);
		assertTrue("Should be readOnly", dateP.isReadOnly());
		try {
			dateP.setValue(new Date());
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		FloatParameter doublP = bagContainer.getParameter(BAG_ID, PARAM_FLOAT_ID, true);
		assertTrue("Should be readOnly", doublP.isReadOnly());
		try {
			doublP.setValue(1.0D);
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		LongParameter lonP = bagContainer.getParameter(BAG_ID, PARAM_LONG_ID, true);
		assertTrue("Should be readOnly", lonP.isReadOnly());
		try {
			lonP.setValue(1L);
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		FloatListParameter doublesP = bagContainer.getParameter(BAG_ID, PARAM_LIST_FLOAT_ID, true);
		assertTrue("Should be readOnly", doublesP.isReadOnly());
		try {
			doublesP.setValue(Collections.singletonList(1.0D));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		IntegerListParameter integersP = bagContainer.getParameter(BAG_ID, PARAM_LIST_INTEGER_ID, true);
		assertTrue("Should be readOnly", integersP.isReadOnly());
		try {
			integersP.setValue(Collections.singletonList(1));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		LongListParameter longsP = bagContainer.getParameter(BAG_ID, PARAM_LIST_LONG_ID, true);
		assertTrue("Should be readOnly", longsP.isReadOnly());
		try {
			longsP.setValue(Collections.singletonList(1L));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		StringListParameter stringsP = bagContainer.getParameter(BAG_ID, PARAM_LIST_STRING_ID, true);
		assertTrue("Should be readOnly", stringsP.isReadOnly());
		try {
			stringsP.setValue(Collections.singletonList("dfgdfg"));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
	}

	private void assertStates(Resource resource) {
		BooleanTimedState booleanState = resource.getTimedState(STATE_BOOLEAN_ID);
		assertTrue("Should be readOnly", booleanState.isReadOnly());
		try {
			booleanState.setStateFromStringAt(1L, "false");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			booleanState.getTimeEvolution().setValueAt(1L, new BooleanValue(false));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		FloatTimedState floatState = resource.getTimedState(STATE_FLOAT_ID);
		assertTrue("Should be readOnly", floatState.isReadOnly());
		try {
			floatState.setStateFromStringAt(1L, "1.0D");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			floatState.getTimeEvolution().setValueAt(1L, new FloatValue(2.0D));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		FloatListTimedState floatListState = resource.getTimedState(STATE_FLOAT_LIST_ID);
		assertTrue("Should be readOnly", floatListState.isReadOnly());
		try {
			floatListState.setStateFromStringAt(1L, "1.0D");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			floatListState.getTimeEvolution().setValueAt(1L, new FloatListValue(2.0D));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		IntegerTimedState integerState = resource.getTimedState(STATE_INTEGER_ID);
		assertTrue("Should be readOnly", integerState.isReadOnly());
		try {
			integerState.setStateFromStringAt(1L, "1");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			integerState.getTimeEvolution().setValueAt(1L, new IntegerValue(1));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}

		StringSetTimedState stringState = resource.getTimedState(STATE_STRING_ID);
		assertTrue("Should be readOnly", stringState.isReadOnly());
		try {
			stringState.setStateFromStringAt(1L, "sd");
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
		try {
			stringState.getTimeEvolution().setValueAt(1L, new StringSetValue("sd"));
			fail("Should have failed as readOnly!");
		} catch (StrolchModelException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("is currently readOnly"));
		}
	}
}
