package li.strolch.report;

import static org.junit.Assert.assertEquals;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.testbase.runtime.RuntimeMock;

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
	public void test() {

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate)) {

			GenericReport report = new GenericReport(tx, "stockReport");
			report.doReportAsJson().forEach(e -> {

				if (e.get("slot").getAsString().equals("Slot 1")) {

					assertEquals("Product 01", e.get("product").getAsString());
					assertEquals("20.0", e.get("quantity").getAsString());
					assertEquals("40.0", e.get("maxQuantity").getAsString());
					assertEquals("Section 001", e.get("section").getAsString());
					assertEquals("Storage 01", e.get("storage").getAsString());
					assertEquals("Location 01", e.get("location").getAsString());

				} else if (e.get("slot").getAsString().equals("Slot 2")) {

					assertEquals("Product 02", e.get("product").getAsString());
					assertEquals("18.0", e.get("quantity").getAsString());
					assertEquals("20.0", e.get("maxQuantity").getAsString());
					assertEquals("Section 001", e.get("section").getAsString());
					assertEquals("Storage 01", e.get("storage").getAsString());
					assertEquals("Location 01", e.get("location").getAsString());

				} else if (e.get("slot").getAsString().equals("Slot 3")) {

					assertEquals("Product 01", e.get("product").getAsString());
					assertEquals("11.0", e.get("quantity").getAsString());
					assertEquals("40.0", e.get("maxQuantity").getAsString());
					assertEquals("Section 002", e.get("section").getAsString());
					assertEquals("Storage 02", e.get("storage").getAsString());
					assertEquals("Location 02", e.get("location").getAsString());

				} else if (e.get("slot").getAsString().equals("Slot 4")) {

					assertEquals("Product 02", e.get("product").getAsString());
					assertEquals("16.0", e.get("quantity").getAsString());
					assertEquals("20.0", e.get("maxQuantity").getAsString());
					assertEquals("Section 002", e.get("section").getAsString());
					assertEquals("Storage 02", e.get("storage").getAsString());
					assertEquals("Location 02", e.get("location").getAsString());
				}
			});
		}
	}
}
