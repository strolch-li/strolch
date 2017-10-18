package li.strolch.report;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.gson.JsonObject;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfSets;

public class GenericReportTest {

	private static final String RUNTIME_PATH = "target/GenericReportTest/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/reporttest"; //$NON-NLS-1$

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
		runtimeMock.logout(certificate);
		runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldGenerateJsonReport() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Report report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			report.doReportAsJson().forEach(e -> {

				if (e.get("slot").getAsString().equals("Slot 1")) {

					assertEquals("Product 01", e.get("product").getAsString());
					assertEquals("20.0", e.get("quantity").getAsString());
					assertEquals("40.0", e.get("maxQuantity").getAsString());
					assertEquals("4.0", e.get("minQuantity").getAsString());
					assertEquals("Section 001", e.get("section").getAsString());
					assertEquals("Storage 01", e.get("storage").getAsString());
					assertEquals("Location 01", e.get("location").getAsString());

				} else if (e.get("slot").getAsString().equals("Slot 2")) {

					assertEquals("Product 02", e.get("product").getAsString());
					assertEquals("18.0", e.get("quantity").getAsString());
					assertEquals("20.0", e.get("maxQuantity").getAsString());
					assertEquals("4.0", e.get("minQuantity").getAsString());
					assertEquals("Section 001", e.get("section").getAsString());
					assertEquals("Storage 01", e.get("storage").getAsString());
					assertEquals("Location 01", e.get("location").getAsString());

				} else if (e.get("slot").getAsString().equals("Slot 3")) {

					assertEquals("Product 01", e.get("product").getAsString());
					assertEquals("11.0", e.get("quantity").getAsString());
					assertEquals("40.0", e.get("maxQuantity").getAsString());
					assertEquals("6.0", e.get("minQuantity").getAsString());
					assertEquals("Section 002", e.get("section").getAsString());
					assertEquals("Storage 02", e.get("storage").getAsString());
					assertEquals("Location 02", e.get("location").getAsString());

				} else if (e.get("slot").getAsString().equals("Slot 4")) {

					assertEquals("Product 02", e.get("product").getAsString());
					assertEquals("16.0", e.get("quantity").getAsString());
					assertEquals("20.0", e.get("maxQuantity").getAsString());
					assertEquals("6.0", e.get("minQuantity").getAsString());
					assertEquals("Section 002", e.get("section").getAsString());
					assertEquals("Storage 02", e.get("storage").getAsString());
					assertEquals("Location 02", e.get("location").getAsString());
				} else {

					fail("Unhandled result element: \n" + e.toString());
				}
			});
		}
	}

	@Test
	public void shouldFilterReport1() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Report report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			report.filter("Product", "product01").doReportAsJson().forEach(e -> {

				String slotName = e.get("slot").getAsString();
				switch (slotName) {
				case "Slot 1":
				case "Slot 3":
					break;
				default:
					fail("Unexpected slot name " + slotName + ", should have been filtered!");
					break;
				}
			});
		}
	}

	@Test
	public void shouldFilterReport2() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Report report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			report.filter("Product", "product01").filter("Location", "location02").doReportAsJson().forEach(e -> {

				String slotName = e.get("slot").getAsString();
				switch (slotName) {
				case "Slot 3":
					break;
				default:
					fail("Unexpected slot name " + slotName + ", should have been filtered!");
					break;
				}
			});
		}
	}

	@Test
	public void shouldFilterReportByDateRange1() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Date from = new Date(LocalDate.of(2016, 1, 1).toEpochDay() * 86400000);
			Date to = new Date(LocalDate.of(2017, 1, 1).toEpochDay() * 86400000);
			DateRange dateRange = new DateRange().from(from, true).to(to, false);

			// expect no slots as all not in date range
			Report report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			List<JsonObject> result = report.filter("Product", "product01").dateRange(dateRange).doReportAsJson()
					.collect(Collectors.toList());
			assertTrue(result.isEmpty());

			// expect 2 slots, as in date range
			to = new Date(LocalDate.of(2017, 3, 1).toEpochDay() * 86400000);
			dateRange = new DateRange().from(from, true).to(to, false);
			report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			result = report.filter("Product", "product01").dateRange(dateRange).doReportAsJson()
					.collect(Collectors.toList());
			assertEquals(2, result.size());
		}
	}

	@Test
	public void shouldFilterReportByDateRange2() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Date from = new Date(LocalDate.of(2016, 1, 1).toEpochDay() * 86400000);
			Date to = new Date(LocalDate.of(2017, 1, 1).toEpochDay() * 86400000);
			DateRange dateRange = new DateRange().from(from, true).to(to, false);

			// expect no orders as all not in date range
			Report report = new Report(runtimeMock.getContainer(), tx, "fromStockReport");
			List<JsonObject> result = report.filter("Product", "product01").dateRange(dateRange).doReportAsJson()
					.collect(Collectors.toList());
			assertTrue(result.isEmpty());

			// expect 2 orders, as in date range
			to = new Date(LocalDate.of(2017, 3, 1).toEpochDay() * 86400000);
			dateRange = new DateRange().from(from, true).to(to, false);
			report = new Report(runtimeMock.getContainer(), tx, "fromStockReport");
			result = report.filter("Product", "product01").dateRange(dateRange).doReportAsJson()
					.collect(Collectors.toList());
			assertEquals(2, result.size());

			// expect 4 orders, as all in date range
			to = new Date(LocalDate.of(2017, 3, 2).toEpochDay() * 86400000);
			dateRange = new DateRange().from(from, true).to(to, false);
			report = new Report(runtimeMock.getContainer(), tx, "fromStockReport");
			result = report.filter("Product", "product01", "product02").dateRange(dateRange).doReportAsJson()
					.collect(Collectors.toList());
			assertEquals(4, result.size());
		}
	}

	@Test
	public void shouldCreateFilterCriteria() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Report report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			MapOfSets<String, StrolchRootElement> filterCriteria = report.generateFilterCriteria();

			assertThat(filterCriteria.getSet("Product").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("product01", "product02"));
			assertThat(filterCriteria.getSet("Location").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("location01", "location02"));
			assertThat(filterCriteria.getSet("Storage").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("storage01", "storage02"));
			assertThat(filterCriteria.getSet("Section").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("section001", "section002"));
			assertFalse(filterCriteria.containsSet("Slot"));
		}
	}

	@Test
	public void shouldCreateFilterCriteriaFiltered1() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Report report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			MapOfSets<String, StrolchRootElement> filterCriteria = report //
					.filter("Product", "product01") //
					.generateFilterCriteria();

			assertThat(filterCriteria.getSet("Product").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("product01"));
			assertThat(filterCriteria.getSet("Location").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("location01", "location02"));
			assertThat(filterCriteria.getSet("Storage").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("storage01", "storage02"));
			assertThat(filterCriteria.getSet("Section").stream().map(e -> e.getId()).collect(Collectors.toSet()),
					containsInAnyOrder("section001", "section002"));
			assertFalse(filterCriteria.containsSet("Slot"));
		}
	}

	@Test
	public void shouldCreateFilterCriteriaFiltered2() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			Date from = new Date(LocalDate.of(2017, 3, 1).toEpochDay() * 86400000);
			Date to = new Date(LocalDate.of(2017, 3, 5).toEpochDay() * 86400000);
			DateRange dateRange = new DateRange().from(from, true).to(to, false);

			Report report = new Report(runtimeMock.getContainer(), tx, "stockReport");
			MapOfSets<String, StrolchRootElement> filterCriteria = report //
					.dateRange(dateRange) //
					.filter("Product", "product01") //
					.generateFilterCriteria();

			assertTrue(filterCriteria.isEmpty());
		}
	}
}
