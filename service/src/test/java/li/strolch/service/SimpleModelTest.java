package li.strolch.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.search.ExpressionsSupport;
import li.strolch.search.OrderSearch;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class SimpleModelTest {

	private static final String RUNTIME_PATH = "target/SimpleModelTest/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/transienttest"; //$NON-NLS-1$

	private static RuntimeMock runtimeMock;
	private static Certificate certificate;

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock().mockRuntime(RUNTIME_PATH, CONFIG_SRC);
		runtimeMock.startContainer();
		certificate = runtimeMock.loginTest();
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null) {
			runtimeMock.logout(certificate);
			runtimeMock.destroyRuntime();
		}
	}

	@Test
	public void shouldDoTx() {

		String customerId;
		String orderId;

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {

			/*
			 * create a new product
			 */
			Resource dafalgan = tx.getResourceTemplate("Product", true);
			dafalgan.setName("Dafalgan 100mg");
			dafalgan.getParameter("description", true).setValue("Dafalgan is for pain.");
			dafalgan.getParameter("color", true).setValue("Yellow");
			dafalgan.getParameter("form", true).setValue("flat");

			StringListParameter articlesP = dafalgan.getRelationsParam("articles", true);

			/*
			 * create articles
			 */
			Resource dafalgan1 = tx.getResourceTemplate("Article", true);
			dafalgan1.setName("Dafalgan 100mg 10 pce");
			dafalgan1.getParameter("description", true).setValue("This is pack with 10 pieces.");
			dafalgan1.getParameter("barcode", true).setValue("654654");

			Resource dafalgan2 = tx.getResourceTemplate("Article", true);
			dafalgan2.setName("Dafalgan 100mg 20 pce");
			dafalgan2.getParameter("description", true).setValue("This is pack with 20 pieces.");
			dafalgan2.getParameter("barcode", true).setValue("654655");

			/*
			 * add reference to product
			 */
			dafalgan1.getRelationParam("product").setValue(dafalgan.getId());
			articlesP.addValue(dafalgan1.getId());
			dafalgan2.getRelationParam("product").setValue(dafalgan.getId());
			articlesP.addValue(dafalgan2.getId());

			/*
			 * create a new customer
			 */
			Resource customer1 = tx.getResourceTemplate("Customer", true);
			customer1.setName("John Doe");

			// set address
			ParameterBag addressBag = customer1.getParameterBag("address", true);
			addressBag.getParameter("street", true).setValue("Main Str. 1");
			addressBag.getParameter("zip", true).setValue("1234");
			addressBag.getParameter("city", true).setValue("Hometown");
			addressBag.getParameter("country", true).setValue("Switzerland");

			/*
			 * create a new order
			 */
			Order order = tx.getOrderTemplate("Order", true);
			order.setName("Order for " + customer1.getName());
			order.setDate(LocalDate.of(2021, 2, 1));
			order.setState(State.PLANNED);

			// store reference to customer
			order.getRelationParam("customer", true).setValue(customer1.getId());

			StringListParameter orderArticlesP = order.getRelationsParam("articles", true);
			ParameterBag quantitiesBag = order.getParameterBag("quantities", true);
			FloatParameter quantityT = quantitiesBag.removeParameter("quantity");

			// order quantity of 20 for Dafalgan 1
			FloatParameter q1P = quantityT.getClone();
			q1P.setId(dafalgan1.getId());
			q1P.setValue(20);
			quantitiesBag.addParameter(q1P);
			orderArticlesP.addValue(dafalgan1.getId());

			// set order quantity of 10 for Dafalgan 2
			FloatParameter q2P = quantityT.getClone();
			orderArticlesP.addValue(dafalgan2.getId());
			q2P.setId(dafalgan2.getId());
			q2P.setValue(20);
			quantitiesBag.addParameter(q2P);

			// keep IDs for use in query in next transaction
			customerId = customer1.getId();
			orderId = order.getId();

			/*
			 * persist
			 */
			tx.add(dafalgan);
			tx.add(dafalgan1);
			tx.add(dafalgan2);
			tx.add(customer1);
			tx.add(order);

			// commit
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, true)) {

			// get order
			Order order = tx.getOrderBy("Order", orderId, true);
			assertNotNull(orderId);
			assertEquals("Order for John Doe", order.getName());

			// get customer
			Resource customer = tx.getResourceByRelation(order, "customer", true);
			assertNotNull(customer);
			assertEquals("John Doe", customer.getName());

			// get articles
			List<Resource> articles = tx.getResourcesByRelation(order, "articles", true);
			assertEquals(2, articles.size());

			// get products
			List<Resource> products = articles.stream().map(a -> tx.getResourceByRelation(a, "product", true))
					.distinct().collect(Collectors.toList());
			assertEquals(1, products.size());

			// search for all orders in state PLANNED and with customer
			List<Order> orders = new OrderSearch().types("Order").stateIsIn(State.PLANNED)
					.where(ExpressionsSupport.relationParam("customer").isEqualTo(customerId)).search(tx).toList();
			assertEquals(1, orders.size());
		}
	}
}
