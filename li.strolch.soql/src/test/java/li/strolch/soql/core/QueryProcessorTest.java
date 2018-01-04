package li.strolch.soql.core;

import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import li.strolch.model.StrolchRootElement;

public class QueryProcessorTest extends BaseTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testProcess() {

		List<StrolchRootElement> resources = getTestRessources(10);
		List<StrolchRootElement> orders = getTestOrders(10);
		
		Map<String, List<StrolchRootElement>> inputCollections = new HashMap<>();
		inputCollections.put("r", resources);
		inputCollections.put("o", orders); 

		QueryProcessor processor = new QueryProcessor();
		processor.setInputCollections(inputCollections);

		QueryRequest request = new QueryRequest();
		request.getParameter().put("id", "0");
		request.setStatement("SELECT r,o FROM Resource r, Order o WHERE r.getId() = :id AND o.getId() = :id");

		QueryResponse response = processor.process(request);
		
		System.out.println(response.asJson());

		// TBD what to assert
	}

}
