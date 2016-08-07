package li.strolch.runtime.query.inmemory;

import static li.strolch.model.query.ParameterSelection.booleanSelection;
import static li.strolch.model.query.ParameterSelection.floatListSelection;
import static li.strolch.model.query.ParameterSelection.floatSelection;
import static li.strolch.model.query.ParameterSelection.integerListSelection;
import static li.strolch.model.query.ParameterSelection.longListSelection;
import static li.strolch.model.query.ParameterSelection.stringListSelection;
import static li.strolch.model.query.ParameterSelection.stringSelection;
import static li.strolch.utils.StringMatchMode.ci;
import static li.strolch.utils.StringMatchMode.es;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Version;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.FloatListParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerListParameter;
import li.strolch.model.parameter.LongListParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.ParameterSelection;
import li.strolch.persistence.inmemory.InMemoryActivityDao;

public class InMemoryActivityQueryTest {

	protected InMemoryActivityDao daoInstance() {
		return new InMemoryActivityDao(false);
	}

	@Test
	public void shouldQueryById() {

		List<Activity> activitys = getActivities();
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType1");
		activityQuery.with(new IdSelection("@1"));

		List<Activity> result = dao.doQuery(activityQuery);
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryByIdOr() {

		List<Activity> activitys = getActivities();
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType2");
		activityQuery.or().with(new IdSelection("@3"), new IdSelection("@4"));

		List<Activity> result = dao.doQuery(activityQuery);
		assertEquals(2, result.size());
		assertEquals("@3", result.get(0).getId());
		assertEquals("@4", result.get(1).getId());
	}

	@Test
	public void shouldQueryByIdAnd() {

		List<Activity> activitys = getActivities();
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType2");
		activityQuery.and().with(new IdSelection("@3"), new NameSelection("Activity 3", es()));

		List<Activity> result = dao.doQuery(activityQuery);
		assertEquals(1, result.size());
		assertEquals("@3", result.get(0).getId());
	}

	@Test
	public void shouldNotQueryByIdAnd() {

		List<Activity> activitys = getActivities();
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> activityQuery = ActivityQuery.query("MyType1");
		activityQuery.and().with(new IdSelection("@3"), new NameSelection("@4", es()));

		List<Activity> result = dao.doQuery(activityQuery);
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByParameter() {

		List<Activity> activitys = getActivities();
		activitys.add(getBallActivity());
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
		ballQuery.and().with(
				//
				stringSelection("parameters", "color", "red", es()),
				booleanSelection("parameters", "forChildren", true), floatSelection("parameters", "diameter", 22.0));

		List<Activity> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByListParameter() {

		List<Activity> activitys = getActivities();
		activitys.add(getBallActivity());
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> ballQuery;
		List<Activity> result;

		// string list
		{
			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a", "z")));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a")));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("c", "b", "a")));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// integer list
		{
			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1, 5)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(3, 2, 1)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// float list
		{
			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0, 8.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(6.2, 5.1, 4.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// long list
		{
			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L, 11L)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ActivityQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(10L, 9L, 8L)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldQueryByNullParameter1() {
		List<Activity> activitys = getActivities();
		activitys.add(getBallActivity());
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "color"));

		List<Activity> result = dao.doQuery(ballQuery);
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByNullParameter2() {
		List<Activity> activitys = getActivities();
		activitys.add(getBallActivity());
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "weight"));

		List<Activity> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByNullParameter3() {
		List<Activity> activitys = getActivities();
		activitys.add(getBallActivity());
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "weight"));

		List<Activity> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByName() {

		List<Activity> activitys = getActivities();
		activitys.add(getBallActivity());
		InMemoryActivityDao dao = daoInstance();
		dao.saveAll(activitys);

		ActivityQuery<Activity> ballQuery = ActivityQuery.query("Ball");
		ballQuery.with(new NameSelection("ball ", ci()));

		List<Activity> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	private Activity getBallActivity() {
		Activity res1 = new Activity("childrensBall", "Ball 1", "Ball");
		Version.setInitialVersionFor(res1, "test");
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

	private List<Activity> getActivities() {
		Activity res1 = ModelGenerator.createActivity("@1", "Activity 1", "MyType1");
		Activity res2 = ModelGenerator.createActivity("@2", "Activity 2", "MyType1");
		Activity res3 = ModelGenerator.createActivity("@3", "Activity 3", "MyType2");
		Activity res4 = ModelGenerator.createActivity("@4", "Activity 4", "MyType2");
		Activity res5 = ModelGenerator.createActivity("@5", "Activity 5", "MyType3");
		Activity res6 = ModelGenerator.createActivity("@6", "Activity 6", "MyType3");
		List<Activity> activitys = new ArrayList<>();
		activitys.add(res1);
		activitys.add(res2);
		activitys.add(res3);
		activitys.add(res4);
		activitys.add(res5);
		activitys.add(res6);

		for (Activity activity : activitys) {
			Version.setInitialVersionFor(activity, "test");
		}

		return activitys;
	}
}
