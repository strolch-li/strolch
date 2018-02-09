package li.strolch.runtime.query.inmemory;

import static li.strolch.agent.ComponentContainerTest.PATH_EMPTY_CONTAINER;
import static li.strolch.model.query.ParameterSelection.*;
import static li.strolch.utils.StringMatchMode.ci;
import static li.strolch.utils.StringMatchMode.es;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.State;
import li.strolch.model.Version;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.*;
import li.strolch.model.query.*;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class InMemoryActivityQueryTest {

	public static final String PATH_RUNTIME = "target/" + InMemoryActivityQueryTest.class.getSimpleName();
	private static RuntimeMock runtimeMock;
	private static Certificate certificate;

	private static Certificate login(StrolchAgent agent) {
		PrivilegeHandler privilegeHandler = agent.getContainer().getPrivilegeHandler();
		return privilegeHandler.authenticate("test", "test".toCharArray());
	}

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock(PATH_RUNTIME, PATH_EMPTY_CONTAINER);
		runtimeMock.mockRuntime();
		runtimeMock.startContainer();

		certificate = login(runtimeMock.getAgent());

		try (StrolchTransaction tx = openTx()) {
			getActivities().forEach(tx::add);
			tx.add(getBallActivity());
			tx.commitOnClose();
		}
	}

	private static StrolchTransaction openTx() {
		return runtimeMock.getAgent().getContainer().getRealm(StrolchConstants.DEFAULT_REALM)
				.openTx(certificate, "test");
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.close();
	}

	@Test
	public void shouldQueryById() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType1");
			activityQuery.with(new IdSelection("@1"));

			List<Activity> result = tx.doQuery(activityQuery);
			assertEquals(1, result.size());
			assertEquals("@1", result.get(0).getId());
		}
	}

	@Test
	public void shouldQueryByIdOr() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType2");
			activityQuery.or().with(new IdSelection("@3"), new IdSelection("@4"));

			List<Activity> result = tx.doQuery(activityQuery);
			assertEquals(2, result.size());
			assertEquals("@3", result.get(0).getId());
			assertEquals("@4", result.get(1).getId());
		}
	}

	@Test
	public void shouldQueryByIdAnd() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType2");
			activityQuery.and().with(new IdSelection("@3"), new NameSelection("Activity 3", es()));

			List<Activity> result = tx.doQuery(activityQuery);
			assertEquals(1, result.size());
			assertEquals("@3", result.get(0).getId());
		}
	}

	@Test
	public void shouldNotQueryByIdAnd() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType1");
			activityQuery.and().with(new IdSelection("@3"), new NameSelection("@4", es()));

			List<Activity> result = tx.doQuery(activityQuery);
			assertEquals(0, result.size());
		}
	}

	@Test
	public void shouldQueryByParameter() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(
					//
					stringSelection("parameters", "color", "red", es()),
					booleanSelection("parameters", "forChildren", true),
					floatSelection("parameters", "diameter", 22.0));

			List<Activity> result = tx.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldQueryByListParameter() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> ballQuery;
			List<Activity> result;

			// string list
			{
				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a", "z")));
				result = tx.doQuery(ballQuery);
				assertEquals(0, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a")));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and()
						.with(stringListSelection("parameters", "stringListValues", Arrays.asList("c", "b", "a")));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());
			}

			// integer list
			{
				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1, 5)));
				result = tx.doQuery(ballQuery);
				assertEquals(0, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1)));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(3, 2, 1)));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());
			}

			// float list
			{
				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0, 8.0)));
				result = tx.doQuery(ballQuery);
				assertEquals(0, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0)));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(6.2, 5.1, 4.0)));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());
			}

			// long list
			{
				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L, 11L)));
				result = tx.doQuery(ballQuery);
				assertEquals(0, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L)));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());

				ballQuery = ActivityQuery.query("Ball");
				ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(10L, 9L, 8L)));
				result = tx.doQuery(ballQuery);
				assertEquals(1, result.size());
			}
		}
	}

	@Test
	public void shouldQueryByNullParameter1() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with( //
					ParameterSelection.nullSelection("parameters", "color"));

			List<Activity> result = tx.doQuery(ballQuery);
			assertEquals(0, result.size());
		}
	}

	@Test
	public void shouldQueryByNullParameter2() {
		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with( //
					ParameterSelection.nullSelection("parameters", "weight"));

			List<Activity> result = tx.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldQueryByNullParameter3() {
		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with( //
					ParameterSelection.nullSelection("parameters", "weight"));

			List<Activity> result = tx.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldQueryByName() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
			ballQuery.with(new NameSelection("ball ", ci()));

			List<Activity> result = tx.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldQueryByState() {

		try (StrolchTransaction tx = openTx()) {

			ActivityQuery<Activity> ballQuery = ActivityQuery.query("MyType1");
			ballQuery.with(new ActivityStateSelection(State.STOPPED));

			List<Activity> result = tx.doQuery(ballQuery);
			assertEquals(2, result.size());

			ballQuery = ActivityQuery.query("MyType2");
			ballQuery.with(new ActivityStateSelection(State.STOPPED));
			result = tx.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	private static Activity getBallActivity() {
		Activity res1 = new Activity("childrensBall", "Ball 1", "Ball", TimeOrdering.SERIES);
		Version.setInitialVersionFor(res1, -1, "test");
		ParameterBag bag = new ParameterBag("parameters", "Ball Details", "Parameters");
		bag.addParameter(new StringParameter("color", "Color", "red"));
		bag.addParameter(new BooleanParameter("forChildren", "Color", true));
		bag.addParameter(new FloatParameter("diameter", "Color", 22.0));
		bag.addParameter(
				new StringListParameter("stringListValues", "List of String Values", Arrays.asList("a", "b", "c")));
		bag.addParameter(new IntegerListParameter("intListValues", "List of Integer Values", Arrays.asList(1, 2, 3)));
		bag.addParameter(
				new FloatListParameter("floatListValues", "List of Float Values", Arrays.asList(4.0, 5.1, 6.2)));
		bag.addParameter(new LongListParameter("longListValues", "List of Long Values", Arrays.asList(8L, 9L, 10L)));
		res1.addParameterBag(bag);
		return res1;
	}

	private static List<Activity> getActivities() {

		Activity activity1 = ModelGenerator.createActivity("@1", "Activity 1", "MyType1", TimeOrdering.SERIES);
		((Action) activity1.getElement("action_" + activity1.getId())).setState(State.STOPPED);

		Activity activity2 = ModelGenerator.createActivity("@2", "Activity 2", "MyType1", TimeOrdering.SERIES);
		((Action) activity2.getElement("action_" + activity2.getId())).setState(State.STOPPED);

		Activity activity3 = ModelGenerator.createActivity("@3", "Activity 3", "MyType2", TimeOrdering.SERIES);
		((Action) activity3.getElement("action_" + activity3.getId())).setState(State.STOPPED);

		Activity activity4 = ModelGenerator.createActivity("@4", "Activity 4", "MyType2", TimeOrdering.SERIES);
		((Action) activity4.getElement("action_" + activity4.getId())).setState(State.PLANNING);

		Activity activity5 = ModelGenerator.createActivity("@5", "Activity 5", "MyType3", TimeOrdering.SERIES);
		((Action) activity5.getElement("action_" + activity5.getId())).setState(State.ERROR);

		Activity activity6 = ModelGenerator.createActivity("@6", "Activity 6", "MyType3", TimeOrdering.SERIES);
		((Action) activity6.getElement("action_" + activity6.getId())).setState(State.CLOSED);

		List<Activity> activities = new ArrayList<>();
		activities.add(activity1);
		activities.add(activity2);
		activities.add(activity3);
		activities.add(activity4);
		activities.add(activity5);
		activities.add(activity6);

		for (Activity activity : activities) {
			Version.setInitialVersionFor(activity, -1, "test");
		}

		return activities;
	}
}
