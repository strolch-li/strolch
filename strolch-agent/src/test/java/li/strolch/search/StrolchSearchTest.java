package li.strolch.search;

import com.google.gson.JsonObject;
import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.iso8601.ISO8601;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.List;
import java.util.Map;

import static java.time.ZoneId.systemDefault;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.ModelGenerator.*;
import static li.strolch.search.ExpressionsSupport.*;
import static li.strolch.search.PredicatesSupport.*;
import static org.junit.Assert.assertEquals;

public class StrolchSearchTest {

	public static final Logger logger = LoggerFactory.getLogger(StrolchSearchTest.class);

	private static final String TARGET_PATH = "target/" + StrolchSearchTest.class.getSimpleName();
	private static final String SOURCE_PATH = "src/test/resources/transienttest";
	public static final String SORTING_TYPE = "SortingType";

	private static RuntimeMock runtimeMock;
	private static Certificate cert;

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock(TARGET_PATH, SOURCE_PATH).mockRuntime();
		runtimeMock.startContainer();
		cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());

		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, false)) {

			{
				Resource ball = createResource("the-id", "Yellow Ball", "Ball");

				ParameterBag bag = new ParameterBag("parameters", "Parameters", "Parameters");
				ball.addParameterBag(bag);

				bag.addParameter(new StringParameter("status", "Status", "bla"));
				bag.addParameter(new StringParameter("color", "Color", "yellow"));

				ball.addParameterBag(new ParameterBag("owner1", "Owner", "Owner"));
				ball.setString("owner1", "name", "Felix");
				ball.addParameterBag(new ParameterBag("owner2", "Owner", "Owner"));
				ball.setString("owner2", "name", "Fox");

				tx.add(ball);
			}

			{
				Resource ball = createResource(getUniqueId(), "Green Ball", "Ball");

				ParameterBag bag = new ParameterBag("parameters", "Parameters", "Parameters");
				ball.addParameterBag(bag);

				bag.addParameter(new StringParameter("status", "Status", "STATUS"));
				bag.addParameter(new StringParameter("color", "Color", "green"));

				bag.addParameter(new StringParameter("state", "State", State.EXECUTION.name()));
				bag.addParameter(new StringListParameter("stateList", "Status",
						asList(State.EXECUTION.name(), State.EXECUTED.name())));

				ball.addParameterBag(new ParameterBag("owner1", "Owner", "Owner"));
				ball.setString("owner1", "name", "Jill");
				ball.addParameterBag(new ParameterBag("owner2", "Owner", "Owner"));
				ball.setString("owner2", "name", "Jane");

				tx.add(ball);
			}

			String id;
			id = "ggg";
			tx.add(createOrder(id, id.toUpperCase(), SORTING_TYPE));
			id = "ccc";
			tx.add(createOrder(id, id.toUpperCase(), SORTING_TYPE));
			id = "aaa";
			tx.add(createOrder(id, id.toUpperCase(), SORTING_TYPE));
			id = "bbb";
			tx.add(createOrder(id, id.toUpperCase(), SORTING_TYPE));
			id = "ddd";
			tx.add(createOrder(id, id.toUpperCase(), SORTING_TYPE));

			tx.commitOnClose();
		}
	}

	@AfterClass
	public static void afterClass() {
		if (cert != null)
			runtimeMock.getPrivilegeHandler().invalidate(cert);
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldSearchResources() {

		StrolchRootElementToJsonVisitor toJsonVisitor = new StrolchRootElementToJsonVisitor();

		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);

		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			List<JsonObject> result = new BallSearch("the-id", "STATUS", "yellow")
					// do search, returns SearchResult
					.search(tx)
					// transform, either:
					.map(a -> a.accept(toJsonVisitor))
					// finishing as toList, toSet, toPaging
					.toList();

			assertEquals(2, result.size());
		}
	}

	@Test
	public void shouldSearchResources1() {

		StrolchRootElementToJsonVisitor toJsonVisitor = new StrolchRootElementToJsonVisitor();

		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);

		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			List<JsonObject> result = new BallSearch("the-id", "STATUS", "yellow")
					.where(element -> element.hasTimedState(STATE_FLOAT_ID))
					.search(tx)
					.map(a -> a.accept(toJsonVisitor))
					.toList();

			assertEquals(2, result.size());
		}
	}

	@Test
	public void shouldSearchResources2() {

		StrolchRootElementToJsonVisitor toJsonVisitor = new StrolchRootElementToJsonVisitor();

		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);

		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			List<JsonObject> result = new NewBallSearch().id("the-id").status("bla").color("yellow")

					// do search, returns SearchResult
					.search(tx)
					// transform, either:
					.map(a -> a.accept(toJsonVisitor))
					// finishing as toList, toSet, toPaging
					.toList();

			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldSearchResources3() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(4, new ResourceSearch()
					.types()
					.where(param(BAG_ID, PARAM_STRING_ID, contains("rol")))
					.search(tx)
					.toList()
					.size());
			assertEquals(4, new ResourceSearch()
					.types()
					.where(param(BAG_ID, PARAM_STRING_ID, startsWithIgnoreCase("STR")))
					.search(tx)
					.toList()
					.size());
		}
	}

	@Test
	public void shouldSearchResources4() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(8, new ResourceSearch().types().search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchResources5() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(2, new ResourceSearch().types("sdf", "Ball").search(tx).toList().size());
			assertEquals(2, new ResourceSearch().types("Ball", "sdf").search(tx).toList().size());
			assertEquals(2, new ResourceSearch().types("4gdf", "Ball", "sdf").search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchResources6() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(1, new ResourceSearch().types("TestType") //
					.where(relationName(tx, "other").isEqualTo("Yellow")) //
					.search(tx).toList().size());
			assertEquals(1, new ResourceSearch().types("TestType") //
					.where(relationName(tx, "other", isEqualTo("Yellow"))) //
					.search(tx).toList().size());

			assertEquals(1, new ResourceSearch().types("TestType") //
					.where(relationParam(tx, "other", "parameters", "color", isEqualTo("yellow"))) //
					.search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchOrders() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			List<Order> result = new OrderSearch() {
				@Override
				public void define() {

					DateRange dateRange = new DateRange()
							.from(ISO8601.parseToZdt("2012-01-01T00:00:00.000+01:00"), true)
							.to(ISO8601.parseToZdt("2013-01-01T00:00:00.000+01:00"), true);

					types().where(date()
							.isEqualTo(Instant.ofEpochMilli(1384929777699L).atZone(systemDefault()))
							.or(state()
									.isEqualTo(State.CREATED)
									.and(param(BAG_ID, PARAM_STRING_ID).isEqualTo("Strolch"))
									.and(param(BAG_ID, PARAM_DATE_ID).inRange(dateRange))));
				}
			}.search(tx).toList();

			assertEquals(7, result.size());
		}
	}

	@Test
	public void shouldSearchOrders1() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			OrderSearch search = new OrderSearch() {
				@Override
				public void define() {
					types(SORTING_TYPE).where(state(isEqualTo(State.CREATED)));
				}
			};

			List<Order> result;

			result = search.search(tx).orderById(true).toList();
			assertEquals(5, result.size());
			assertEquals("ggg", result.getFirst().getId());

			result = search.search(tx).orderByName(false).toList();
			assertEquals(5, result.size());
			assertEquals("aaa", result.getFirst().getId());
		}
	}

	@Test
	public void shouldSearchOrders2() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(5, new OrderSearch().types("sdf", SORTING_TYPE).search(tx).toList().size());
			assertEquals(5, new OrderSearch().types(SORTING_TYPE, "sdf").search(tx).toList().size());
			assertEquals(5, new OrderSearch().types("4gdf", SORTING_TYPE, "sdf").search(tx).toList().size());
			assertEquals(7, new OrderSearch().types().search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchOrders3() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			ZonedDateTime dateTime = ISO8601.parseToZdt("2013-11-20T07:42:57.699+01:00");
			assertEquals(0, new OrderSearch()
					.types("TestType")
					.where(date().isBefore(dateTime, false))
					.search(tx)
					.toList()
					.size());
			assertEquals(1, new OrderSearch()
					.types("TestType")
					.where(date().isBefore(dateTime, true))
					.search(tx)
					.toList()
					.size());
			assertEquals(0, new OrderSearch()
					.types("TestType")
					.where(date().isAfter(dateTime, false))
					.search(tx)
					.toList()
					.size());
			assertEquals(1, new OrderSearch()
					.types("TestType")
					.where(date().isAfter(dateTime, true))
					.search(tx)
					.toList()
					.size());
		}
	}

	@Test
	public void shouldSearchActivities() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			Map<String, State> states = new ActivitySearch() {
				@Override
				public void define() {
					types().where(state().isEqualTo(State.PLANNING).and(name(isEqualTo("Activity"))));
				}
			}.search(tx).toMap(Activity::getId, Activity::getState);

			assertEquals(1, states.size());
		}
	}

	@Test
	public void shouldSearchActivities1() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			Map<String, State> states = new ActivitySearch()

					.types().where(state().isEqualTo(State.PLANNING).and(name(isEqualTo("Activity"))))

					.search(tx).toMap(Activity::getId, Activity::getState);

			assertEquals(1, states.size());
		}
	}

	@Test
	public void shouldSearchActivities2() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(2, new ActivitySearch().types("sdf", "ActivityType").search(tx).toList().size());
			assertEquals(2, new ActivitySearch().types("ActivityType", "sdf").search(tx).toList().size());
			assertEquals(2, new ActivitySearch().types("4gdf", "ActivityType", "sdf").search(tx).toList().size());
			assertEquals(2, new ActivitySearch().types().search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchActivities3() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(1, new ActivitySearch()
					.types("sdf", "ActivityType")
					.where(element -> element.getActionsByType("Use").size() == 4)
					.search(tx)
					.toList()
					.size());
		}
	}

	@Test
	public void shouldSearchRootElements() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, StrolchSearchTest.class, true)) {

			assertEquals(9,
					new RootElementSearch().types(SORTING_TYPE, "Ball", "ActivityType").search(tx).toList().size());
			assertEquals(17, new RootElementSearch().types().search(tx).toList().size());
			assertEquals(2, new RootElementSearch().types("ActivityType").search(tx).toList().size());
		}
	}

	public static class NewBallSearch extends ResourceSearch {

		@Override
		protected void define() {
			types("Ball");
		}

		public NewBallSearch id(String id) {
			where(id(isEqualTo(id)));
			return this;
		}

		public NewBallSearch status(String status) {
			where(param("parameters", "status", isEqualTo(status)));
			return this;
		}

		public NewBallSearch color(String color) {
			where(param("parameters", "color", isEqualTo(color)));
			return this;
		}
	}

	public static class BallSearch extends ResourceSearch {

		private final String id;
		private final String status;
		private final String color;

		public BallSearch(String id, String status, String color) {
			this.id = id;
			this.status = status;
			this.color = color;
		}

		@Override
		public void define() {
			types("Ball").where(id(isEqualTo(this.id)).or( //

					param("parameters", "status", isEqualTo(this.status))
							.and(not(param("parameters", "color", isEqualTo(this.color))))

							.and(param("parameters", "state", isEqualTo(State.EXECUTION)))
							.and(param("parameters", "state", isEqualToIgnoreCase(State.EXECUTION)))
							.and(param("parameters", "state", isIn(State.EXECUTION, State.CREATED)))
							.and(param("parameters", "state", contains(State.EXECUTION)))
							.and(param("parameters", "state", containsIgnoreCase(State.EXECUTION)))
							.and(param("parameters", "stateList", listContains(State.EXECUTED)))

							.and(param(BAG_ID, PARAM_FLOAT_ID, isEqualTo(44.3D)))

							.and(param(BAG_ID, PARAM_STRING_ID, isEqualTo("Strolch")))
							.and(param(BAG_ID, PARAM_STRING_ID, isEqualToIgnoreCase("strolch")))
							.and(param(BAG_ID, PARAM_STRING_ID, isNotEqualTo("dfgdfg")))
							.and(param(BAG_ID, PARAM_STRING_ID, isNotEqualToIgnoreCase("dfgdfg")))
							.and(param(BAG_ID, PARAM_STRING_ID, contains("rol")))
							.and(param(BAG_ID, PARAM_STRING_ID, isIn("Strolch")))
							.and(param(BAG_ID, PARAM_STRING_ID, isIn("Strolch", "sdf")))
							.and(param(BAG_ID, PARAM_STRING_ID, isInIgnoreCase("strolch")))
							.and(param(BAG_ID, PARAM_STRING_ID, isInIgnoreCase("strolch", "dfgdfg")))
							.and(param(BAG_ID, PARAM_STRING_ID, contains(new String[]{"Str", "rol"})))
							.and(param(BAG_ID, PARAM_STRING_ID, containsIgnoreCase("ROL")))
							.and(param(BAG_ID, PARAM_STRING_ID, containsIgnoreCase(new String[]{"STR", "ROL"})))
							.and(param(BAG_ID, PARAM_STRING_ID, startsWith("Str")))
							.and(param(BAG_ID, PARAM_STRING_ID, startsWithIgnoreCase("str")))
							.and(param(BAG_ID, PARAM_STRING_ID, endsWith("lch")))
							.and(param(BAG_ID, PARAM_STRING_ID, endsWithIgnoreCase("LCH")))

							.and(param(BAG_ID, PARAM_BOOLEAN_ID, isEqualTo(true)))
							.and(param(BAG_ID, PARAM_BOOLEAN_ID, isEqualTo("true")))
							.and(param(BAG_ID, PARAM_DATE_ID, isEqualTo(new Date(1354295525628L))))
							.and(param(BAG_ID, PARAM_DATE_ID,
									isEqualTo(ISO8601FormatFactory.getInstance().formatDate(new Date(1354295525628L)))))

							.and(param(BAG_ID, PARAM_INTEGER_ID, isEqualTo(77))) //
							.and(param(BAG_ID, PARAM_INTEGER_ID, isIn(77))) //
							.and(param(BAG_ID, PARAM_INTEGER_ID, isIn("77"))) //
							.and(param(BAG_ID, PARAM_INTEGER_ID, isIn(77, 88))) //
							.and(param(BAG_ID, PARAM_INTEGER_ID, isIn(asList(77, 88)))) //

							.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, isEqualTo(asList(6.0D, 11.0D, 16.0D))))
							.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, contains(singletonList(6.0D))))
							.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, contains(asList(6.0D, 11.0D))))
							.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, contains("6.0D,11.0D")))

							.and(param(BAG_ID, PARAM_LIST_INTEGER_ID, isEqualTo(asList(5, 10, 15))))
							.and(param(BAG_ID, PARAM_LIST_INTEGER_ID, isIn(asList(5, 10))))
							.and(param(BAG_ID, PARAM_LIST_INTEGER_ID, contains(asList(5, 10))))

							.and(param(BAG_ID, PARAM_LIST_LONG_ID, isEqualTo(asList(7L, 12L, 17L))))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, isEqualTo(asList("Hello", "World"))))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, isEqualToIgnoreCase(asList("hello", "world"))))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, contains(new String[]{"Hel", "wor"})))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, containsIgnoreCase(new String[]{"Hel", "wor"})))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, containsIgnoreCase(new String[]{"hel"})))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, isIn(asList("Hello", "World"))))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, isIn(asList("Hello", "World", "Extra"))))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, isIn(asList("Extra", "Sauce")).not()))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, listContains("Hello")))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, listContains("Hello1")).not())
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, listContains("World")))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, listContains("World1")).not())

							.and(paramOnBagType("Owner", "name").isInArray("Felix", "Jill"))

							.and(paramNull(BAG_ID, "non-existant"))
					//
			));
		}
	}
}
