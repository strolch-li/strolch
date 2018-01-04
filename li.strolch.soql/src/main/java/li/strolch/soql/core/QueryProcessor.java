package li.strolch.soql.core;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchElementQuery;
import li.strolch.soql.antlr4.generated.SOQLLexer;
import li.strolch.soql.antlr4.generated.SOQLParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.util.*;

/**
 * Main class to process a SOQL query. It performs the queries to retrieve the
 * input objects as defined in the FROM clause and evaluates the compiled
 * statement on any cartesian product, returning the objects defined in the
 * SELECT clause.
 *
 * @author msmock
 */
public class QueryProcessor {

	// Map of queries to retrieve the input objects
	protected Map<String, StrolchElementQuery> queries;

	// Map of entities the input objects are taken from
	protected Map<String, List<StrolchRootElement>> inputCollections;

	/**
	 * empty constructor
	 */
	public QueryProcessor() {
	}

	/**
	 * Set the input map of collections to take the input objects from. For testing
	 * purposes only.
	 *
	 * @param inputCollections
	 */
	void setInputCollections(Map<String, List<StrolchRootElement>> inputCollections) {
		this.inputCollections = inputCollections;
	}

	/**
	 * @param request
	 * @return the query response object covering the result set
	 */
	public QueryResponse process(QueryRequest request) {

		String statement = request.getStatement();

		CompiledStatement compiledStatement = null;
		try {
			ParseTree tree = parseStatement(statement);
			compiledStatement = compile(tree);
		} catch (Exception e) {
			// TODO add error handling
			e.printStackTrace();
			throw new SOQLParseException(e.getMessage());
		}

		// build the input collections, if not set already
		if (inputCollections == null) {
			queries = entity2Query(compiledStatement.entities);
			// TODO perform the queries
		}

		// build cartesian product
		Object[] keys = inputCollections.keySet().toArray(); 
		List<List<StrolchRootElement>> cartesianProduct = buildCartesianProduct(inputCollections, keys);

		ResultSet resultSet = new ResultSet(); 
		
		// apply the compiled statement to any row of the cartesian product
		for (List<StrolchRootElement> row : cartesianProduct) {
			Map<String, Object> inputObjects = new HashMap<>();
			for (int i = 0; i < row.size(); i++) {
				inputObjects.put((String) keys[i], row.get(i));
			}
			List<Object> resultRow = compiledStatement.evaluate(inputObjects, request.getParameter());

			if (!resultRow.isEmpty())
				resultSet.add(resultRow);
		}
		
		QueryResponse response = new QueryResponse();
		response.setStatement(statement);
		response.setParameter(request.getParameter());
		response.setResultSet(resultSet); 

		return response;

	}

	/**
	 * @param inputCollections
	 * @param keys
	 * @return List of Lists of the elements to be taken as input for the compiled statement
	 */
	private List<List<StrolchRootElement>> buildCartesianProduct(Map<String, List<StrolchRootElement>> inputCollections, Object[] keys) {
		
		int numberOfKeys = keys.length;

		// get an overview of how many elements we have to take into account
		int[] numberOfEntities = new int[numberOfKeys];
		for (int keyIndex = 0; keyIndex < numberOfKeys; keyIndex++) {
			numberOfEntities[keyIndex] = inputCollections.get(keys[keyIndex]).size();
		}

		// build cartesian product
		List<List<StrolchRootElement>> cartesianProduct = new ArrayList<>();
		IndexPointer indexPointer = new IndexPointer(numberOfEntities);
		while (indexPointer.hasNext()) {

			List<StrolchRootElement> row = new ArrayList<>();
			cartesianProduct.add(row);

			int[] pointer = indexPointer.next();

			// fasten your seat belts, here we go
			for (int keyIndex = 0; keyIndex < numberOfKeys; keyIndex++) {
				Object key = keys[keyIndex];
				List<StrolchRootElement> elements = inputCollections.get(key);
				StrolchRootElement element = elements.get(pointer[keyIndex]);
				row.add(element);
			}
		}
		return cartesianProduct;
	}

	/**
	 * parse the string and return the antlr tree
	 *
	 * @throws Exception
	 */
	ParseTree parseStatement(String s) throws Exception {

		// build a buffer of tokens pulled from the lexer
		CharStream input = CharStreams.fromString(s);
		SOQLLexer lexer = new SOQLLexer(input);

		// build a parser that feeds off the tokens buffer
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		SOQLParser parser = new SOQLParser(tokens);
		parser.addErrorListener(new VerboseListener());

		return parser.select_statement();
	}

	/**
	 * compile the antlr tree to executeable code
	 *
	 * @param tree
	 * @return CompiledSOQLStatement
	 * @throws Exception
	 */
	CompiledStatement compile(ParseTree tree) throws Exception {

		ParseTreeWalker walker = new ParseTreeWalker();
		SOQLListener listener = new SOQLListener();
		walker.walk(listener, tree);

		CompiledStatement soqlStatement = new CompiledStatement();
		soqlStatement.entities = listener.getEntities();
		soqlStatement.whereExpression = listener.getWhereExpression();
		soqlStatement.selectClause = listener.getSelectClause();

		return soqlStatement;
	}

	/**
	 * Resolve the entities extracted from the FROM clause to the
	 * StrolchElementQuery to be performed to retrieve the input objects.
	 *
	 * @param entities
	 * @return Map of queries for the entities defining the objects returned
	 */
	Map<String, StrolchElementQuery> entity2Query(Map<String, String> entities) {

		Map<String, StrolchElementQuery> result = new HashMap<>();

		Set<String> keys = entities.keySet();
		for (String key : keys) {
			String clazzKey = entities.get(key);

			switch (clazzKey) {
			case "Resource":
				result.put(key, new ResourceQuery());
				break;
			case "Order":
				result.put(key, new OrderQuery());
				break;
			case "Activity":
				result.put(key, new ActivityQuery());
				break;
			default:
				String s = "Unable to resolve " + clazzKey + " to strolch entity query.";
				throw new SOQLParseException(s);
			}
		}
		return result;
	}

}
