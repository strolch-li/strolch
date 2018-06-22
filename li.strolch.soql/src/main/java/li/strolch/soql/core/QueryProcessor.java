package li.strolch.soql.core;

import java.util.*;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.query.StrolchElementQuery;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.search.ActivitySearch;
import li.strolch.search.OrderSearch;
import li.strolch.search.ResourceSearch;
import li.strolch.soql.antlr4.generated.SOQLLexer;
import li.strolch.soql.antlr4.generated.SOQLParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

/**
 * Main class to process a SOQL query. It performs the queries to retrieve the input objects as defined in the FROM
 * clause and evaluates the compiled statement on any cartesian product, returning the objects defined in the SELECT
 * clause.
 *
 * @author msmock
 */
public class QueryProcessor {

	// Map of queries to retrieve the input objects
	protected Map<String, StrolchElementQuery<?>> queries;

	// Map of entities the input objects are taken from
	private Map<String, List<? extends StrolchRootElement>> inputCollections;

	/**
	 * Set the input map of collections to take the input objects from. For testing purposes only.
	 *
	 * @param inputCollections
	 * 		the input data
	 */
	void setInputCollections(Map<String, List<? extends StrolchRootElement>> inputCollections) {
		this.inputCollections = inputCollections;
	}

	/**
	 * @param request
	 * 		the query request
	 *
	 * @return the query response object covering the result set
	 */
	public QueryResponse process(QueryRequest request, StrolchTransaction tx) {

		String statement = request.getStatement();
		Map<String, Object> parameterMap = request.getParameterMap();

		CompiledStatement compiledStatement;
		try {
			ParseTree tree = parseStatement(statement);
			compiledStatement = compile(tree);
		} catch (Exception e) {
			throw new SOQLParseException("Failed to parse String " + statement, e);
		}

		// build the input collections, if not set already
		if (this.inputCollections == null)
			this.inputCollections = queryInputCollection(compiledStatement.entities, tx);

		// build cartesian product
		Object[] keys = this.inputCollections.keySet().toArray();
		List<List<StrolchRootElement>> cartesianProduct = buildCartesianProduct(this.inputCollections, keys);

		ResultSet resultSet = new ResultSet();

		// apply the compiled statement to any row of the cartesian product
		for (List<StrolchRootElement> row : cartesianProduct) {
			Map<String, Object> inputObjects = new HashMap<>();
			for (int i = 0; i < row.size(); i++) {
				inputObjects.put((String) keys[i], row.get(i));
			}
			List<Object> resultRow = compiledStatement.evaluate(inputObjects, parameterMap);

			if (!resultRow.isEmpty())
				resultSet.add(resultRow);
		}

		QueryResponse response = new QueryResponse();
		response.setStatement(statement);
		response.setParameterMap(parameterMap);
		response.setResultSet(resultSet);

		return response;
	}

	/**
	 * @param inputCollections
	 * 		the data
	 * @param keys
	 * 		the keys
	 *
	 * @return List of Lists of the elements to be taken as input for the compiled statement
	 */
	private List<List<StrolchRootElement>> buildCartesianProduct(
			Map<String, List<? extends StrolchRootElement>> inputCollections, Object[] keys) {

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
				List<? extends StrolchRootElement> elements = inputCollections.get(key);
				StrolchRootElement element = elements.get(pointer[keyIndex]);
				row.add(element);
			}
		}
		return cartesianProduct;
	}

	/**
	 * parse the string and return the antlr tree
	 */
	private ParseTree parseStatement(String s) {

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
	 * compile the antlr tree to executable code
	 *
	 * @param tree
	 * 		the tree to compile to a statement
	 *
	 * @return CompiledSOQLStatement
	 */
	private CompiledStatement compile(ParseTree tree) {

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
	 * Query all strolch root elements declared in the FROM clause of the query
	 *
	 * @param entities
	 * 		the entity types to query
	 *
	 * @return the input collection
	 */
	private Map<String, List<? extends StrolchRootElement>> queryInputCollection(Map<String, String> entities,
			StrolchTransaction tx) {

		Map<String, List<? extends StrolchRootElement>> result = new HashMap<>();

		Set<String> keys = entities.keySet();
		for (String key : keys) {
			String clazzKey = entities.get(key);

			switch (clazzKey) {
			case Tags.RESOURCE:
				result.put(key, new ResourceSearch().search(tx).toList());
				break;

			case Tags.ORDER:
				result.put(key, new OrderSearch().search(tx).toList());
				break;

			case Tags.ACTIVITY:
				result.put(key, new ActivitySearch().search(tx).toList());
				break;

			default:
				String s = "Unable to resolve " + clazzKey + " " + key + " to strolch root entities.";
				throw new SOQLParseException(s);
			}
		}

		return result;
	}
}
