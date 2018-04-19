package li.strolch.search;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.ModelGenerator.*;
import static li.strolch.search.ExpressionsSupport.*;
import static li.strolch.search.PredicatesSupport.*;
import static org.junit.Assert.assertEquals;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gson.JsonObject;
import li.strolch.RuntimeMock;
import li.strolch.agent.ParallelTests;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StrolchSearchTest {

	public static final Logger logger = LoggerFactory.getLogger(StrolchSearchTest.class);

	private static final String TARGET_PATH = "target/" + StrolchSearchTest.class.getSimpleName();
	private static final String SOURCE_PATH = "src/test/resources/transienttest";

	private static RuntimeMock runtimeMock;
	private static Certificate cert;

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock(TARGET_PATH, SOURCE_PATH).mockRuntime();
		runtimeMock.startContainer();
		cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());

		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			{
				Resource ball = createResource("the-id", "Yellow Ball", "Ball");

				ParameterBag bag = new ParameterBag("parameters", "Parameters", "Parameters");
				ball.addParameterBag(bag);

				bag.addParameter(new StringParameter("status", "Status", "bla"));
				bag.addParameter(new StringParameter("color", "Color", "yellow"));

				tx.add(ball);
			}

			{
				Resource ball = createResource(getUniqueId(), "Green Ball", "Ball");

				ParameterBag bag = new ParameterBag("parameters", "Parameters", "Parameters");
				ball.addParameterBag(bag);

				bag.addParameter(new StringParameter("status", "Status", "STATUS"));
				bag.addParameter(new StringParameter("color", "Color", "green"));

				tx.add(ball);
			}

			String id;
			id = "ggg";
			tx.add(createOrder(id, id.toUpperCase(), "SortingType"));
			id = "ccc";
			tx.add(createOrder(id, id.toUpperCase(), "SortingType"));
			id = "aaa";
			tx.add(createOrder(id, id.toUpperCase(), "SortingType"));
			id = "bbb";
			tx.add(createOrder(id, id.toUpperCase(), "SortingType"));
			id = "ddd";
			tx.add(createOrder(id, id.toUpperCase(), "SortingType"));

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

		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

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

		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			List<JsonObject> result = new BallSearch("the-id", "STATUS", "yellow")
					.where(element -> element.hasTimedState(STATE_FLOAT_ID)).search(tx)
					.map(a -> a.accept(toJsonVisitor)).toList();

			assertEquals(2, result.size());
		}
	}

	@Test
	public void shouldSearchResources2() {

		StrolchRootElementToJsonVisitor toJsonVisitor = new StrolchRootElementToJsonVisitor();

		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);

		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

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
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			assertEquals(4,
					new ResourceSearch().types().where(param(BAG_ID, PARAM_STRING_ID, contains("rol"))).search(tx)
							.toList().size());
			assertEquals(4,
					new ResourceSearch().types().where(param(BAG_ID, PARAM_STRING_ID, startsWithIgnoreCase("STR")))
							.search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchResources4() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			assertEquals(7, new ResourceSearch().types().search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchResources5() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			assertEquals(2, new ResourceSearch().types("sdf", "Ball").search(tx).toList().size());
			assertEquals(2, new ResourceSearch().types("Ball", "sdf").search(tx).toList().size());
			assertEquals(2, new ResourceSearch().types("4gdf", "Ball", "sdf").search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchOrders() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			List<Order> result = new OrderSearch() {
				@Override
				public void define() {

					DateRange dateRange = new DateRange()
							.from(ISO8601FormatFactory.getInstance().parseDate("2012-01-01T00:00:00.000+01:00"), true)
							.to(ISO8601FormatFactory.getInstance().parseDate("2013-01-01T00:00:00.000+01:00"), true);

					types().where(date(isEqualTo(new Date(1384929777699L))).or(state(isEqualTo(State.CREATED))
							.and(param(BAG_ID, PARAM_STRING_ID, isEqualTo("Strolch")))
							.and(param(BAG_ID, PARAM_DATE_ID, inRange(dateRange)))));
				}
			}.search(tx).toList();

			assertEquals(7, result.size());
		}
	}

	@Test
	public void shouldSearchOrders1() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			StrolchSearch<Order> search = new OrderSearch() {
				@Override
				public void define() {
					types("SortingType").where(state(isEqualTo(State.CREATED)));
				}
			};

			List<Order> result;

			result = search.search(tx).orderById(true).toList();
			assertEquals(5, result.size());
			assertEquals("ggg", result.get(0).getId());

			result = search.search(tx).orderByName(false).toList();
			assertEquals(5, result.size());
			assertEquals("aaa", result.get(0).getId());
		}
	}

	@Test
	public void shouldSearchOrders2() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			assertEquals(5, new OrderSearch().types("sdf", "SortingType").search(tx).toList().size());
			assertEquals(5, new OrderSearch().types("SortingType", "sdf").search(tx).toList().size());
			assertEquals(5, new OrderSearch().types("4gdf", "SortingType", "sdf").search(tx).toList().size());
			assertEquals(7, new OrderSearch().types().search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchActivities() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

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
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			Map<String, State> states = new ActivitySearch()

					.types().where(state().isEqualTo(State.PLANNING).and(name(isEqualTo("Activity"))).asActivityExp())

					.search(tx).toMap(Activity::getId, Activity::getState);

			assertEquals(1, states.size());
		}
	}

	@Test
	public void shouldSearchActivities2() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			assertEquals(2, new ActivitySearch().types("sdf", "ActivityType").search(tx).toList().size());
			assertEquals(2, new ActivitySearch().types("ActivityType", "sdf").search(tx).toList().size());
			assertEquals(2, new ActivitySearch().types("4gdf", "ActivityType", "sdf").search(tx).toList().size());
			assertEquals(2, new ActivitySearch().types().search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchActivities3() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			assertEquals(1, new ActivitySearch().types("sdf", "ActivityType")
					.where(element -> element.getActionsByType("Use").size() == 4).search(tx).toList().size());
		}
	}

	@Test
	public void shouldSearchRootElements() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			assertEquals(9,
					new RootElementSearch().types("SortingType", "Ball", "ActivityType").search(tx).toList().size());
			assertEquals(16, new RootElementSearch().types().search(tx).toList().size());
			assertEquals(2, new RootElementSearch().types("ActivityType").search(tx).toList().size());
		}
	}

	public class NewBallSearch extends ResourceSearch {

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

	public class BallSearch extends ResourceSearch {

		private String id;
		private String status;
		private String color;

		public BallSearch(String id, String status, String color) {
			this.id = id;
			this.status = status;
			this.color = color;
		}

		@Override
		public void define() {
			types("Ball").where(id(isEqualTo(this.id)).or(param("parameters", "status", isEqualTo(this.status))
							.and(not(param("parameters", "color", isEqualTo(this.color))))

							.and(param(BAG_ID, PARAM_FLOAT_ID, isEqualTo(44.3D)))

							.and(param(BAG_ID, PARAM_STRING_ID, isEqualTo("Strolch")))
							.and(param(BAG_ID, PARAM_STRING_ID, isEqualToIgnoreCase("strolch")))
							.and(param(BAG_ID, PARAM_STRING_ID, isNotEqualTo("dfgdfg")))
							.and(param(BAG_ID, PARAM_STRING_ID, isNotEqualToIgnoreCase("dfgdfg")))
							.and(param(BAG_ID, PARAM_STRING_ID, contains("rol")))
							.and(param(BAG_ID, PARAM_STRING_ID, contains(new String[] { "Str", "rol" })))
							.and(param(BAG_ID, PARAM_STRING_ID, containsIgnoreCase("ROL")))
							.and(param(BAG_ID, PARAM_STRING_ID, containsIgnoreCase(new String[] { "STR", "ROL" })))
							.and(param(BAG_ID, PARAM_STRING_ID, startsWith("Str")))
							.and(param(BAG_ID, PARAM_STRING_ID, startsWithIgnoreCase("str")))
							.and(param(BAG_ID, PARAM_STRING_ID, endsWith("lch")))
							.and(param(BAG_ID, PARAM_STRING_ID, endsWithIgnoreCase("LCH")))

							.and(param(BAG_ID, PARAM_BOOLEAN_ID, isEqualTo(true)))
							.and(param(BAG_ID, PARAM_DATE_ID, isEqualTo(new Date(1354295525628L))))
							.and(param(BAG_ID, PARAM_INTEGER_ID, isEqualTo(77)))

							.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, isEqualTo(asList(6.0D, 11.0D, 16.0D))))
							.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, contains(singletonList(6.0D))))

							.and(param(BAG_ID, PARAM_LIST_INTEGER_ID, isEqualTo(asList(5, 10, 15))))
							.and(param(BAG_ID, PARAM_LIST_LONG_ID, isEqualTo(asList(7L, 12L, 17L))))
							.and(param(BAG_ID, PARAM_LIST_STRING_ID, isEqualTo(asList("Hello", "World"))))

							.and(paramNull(BAG_ID, "non-existant"))
					//
			));
		}
	}
}
