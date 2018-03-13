package li.strolch.search;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.assertEquals;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gson.JsonObject;
import li.strolch.RuntimeMock;
import li.strolch.agent.ParallelTests;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.*;
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

	public static final Logger logger = LoggerFactory.getLogger(ParallelTests.class);

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
					.asResources() //  asOrders(), asActivities() or map to something entirely different:
					.map(a -> a.accept(toJsonVisitor))
					// finishing as toList, toSet, toPaging
					.toList();

			assertEquals(2, result.size());
		}
	}

	@Test
	public void shouldSearchOrders() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			List<Order> result = new StrolchSearch() {
				@Override
				public StrolchSearch prepare() {

					DateRange dateRange = new DateRange() //
							.from(ISO8601FormatFactory.getInstance().parseDate("2012-01-01T00:00:00.000+01:00"), true)
							.to(ISO8601FormatFactory.getInstance().parseDate("2013-01-01T00:00:00.000+01:00"), true);

					return orders() //
							.where(date(isEqualTo(new Date(1384929777699L))) //
									.or(state(isEqualTo(State.CREATED)) //
											.and(param(BAG_ID, PARAM_STRING_ID, isEqualTo("Strolch"))) //
											.and(param(BAG_ID, PARAM_DATE_ID, inRange(dateRange)))));
				}
			} //
					.search(tx) //
					.asOrders() //
					.toList();

			assertEquals(2, result.size());
		}
	}

	@Test
	public void shouldSearchOrders1() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			StrolchSearch search = new StrolchSearch() {
				@Override
				public StrolchSearch prepare() {
					return orders("SortingType").where(state(isEqualTo(State.CREATED)));
				}
			};

			List<StrolchRootElement> result;

			result = search //
					.search(tx) //
					.orderById(true) //
					.toList();
			assertEquals(5, result.size());
			assertEquals("ggg", result.get(0).getId());

			result = search //
					.search(tx) //
					.orderByName(false) //
					.toList();
			assertEquals(5, result.size());
			assertEquals("aaa", result.get(0).getId());
		}
	}

	@Test
	public void shouldSearchActivities() {
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			Map<String, State> states = new StrolchSearch() {
				@Override
				public StrolchSearch prepare() {
					return activities() //
							.where(state(isEqualTo(State.PLANNING)) //
									.and(name(isEqualTo("Activity"))) //
							);
				}
			} //
					.search(tx) //
					.asActivities() //
					.toMap(Activity::getId, Activity::getState);

			assertEquals(1, states.size());
		}
	}

	public class BallSearch extends StrolchSearch {

		private String id;
		private String status;
		private String color;

		public BallSearch(String id, String status, String color) {
			this.id = id;
			this.status = status;
			this.color = color;
		}

		@Override
		public StrolchSearch prepare() {
			return resources("Ball") //
					.where(id(isEqualTo(this.id))  //
							.or(param("parameters", "status", isEqualTo(this.status))  //
									.and(not(param("parameters", "color", isEqualTo(this.color)))) //

									.and(param(BAG_ID, PARAM_FLOAT_ID, isEqualTo(44.3D))) //

									.and(param(BAG_ID, PARAM_STRING_ID, isEqualTo("Strolch"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, isEqualToIgnoreCase("strolch"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, isNotEqualTo("dfgdfg"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, isNotEqualToIgnoreCase("dfgdfg"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, contains("rol"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, containsIgnoreCase("ROL"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, startsWith("Str"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, startsWithIgnoreCase("str"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, endsWith("lch"))) //
									.and(param(BAG_ID, PARAM_STRING_ID, endsWithIgnoreCase("LCH"))) //

									.and(param(BAG_ID, PARAM_BOOLEAN_ID, isEqualTo(true))) //
									.and(param(BAG_ID, PARAM_DATE_ID, isEqualTo(new Date(1354295525628L)))) //
									.and(param(BAG_ID, PARAM_INTEGER_ID, isEqualTo(77))) //

									.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, isEqualTo(asList(6.0D, 11.0D, 16.0D)))) //
									.and(param(BAG_ID, PARAM_LIST_FLOAT_ID, contains(singletonList(6.0D)))) //

									.and(param(BAG_ID, PARAM_LIST_INTEGER_ID, isEqualTo(asList(5, 10, 15)))) //
									.and(param(BAG_ID, PARAM_LIST_LONG_ID, isEqualTo(asList(7L, 12L, 17L)))) //
									.and(param(BAG_ID, PARAM_LIST_STRING_ID, isEqualTo(asList("Hello", "World")))) //

									.and(paramNull(BAG_ID, "non-existant")) //
							));
		}
	}
}
