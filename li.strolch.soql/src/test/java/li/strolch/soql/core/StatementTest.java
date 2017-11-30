package li.strolch.soql.core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;

import li.strolch.soql.antlr4.generated.SOQLLexer;
import li.strolch.soql.antlr4.generated.SOQLParser;

public class StatementTest {

	/**
	 * @throws Exception
	 */
	private ParseTree parseString(final String s) throws Exception {

		CharStream input = CharStreams.fromString(s);
		SOQLLexer lexer = new SOQLLexer(input); // create a buffer of tokens pulled from the lexer

		CommonTokenStream tokens = new CommonTokenStream(lexer); // create a parser that feeds off the tokens buffer
		SOQLParser parser = new SOQLParser(tokens);

		ParseTree tree = parser.select_statement(); // begin parsing at block

		System.out.println(tree.toStringTree(parser)); // print LISP-style tree

		return tree;
	}

	/**
	 * @param tree
	 * @return CompiledSOQLStatement the compiled SOQL statement
	 * @throws Exception
	 */
	private CompiledStatement compile(final ParseTree tree) throws Exception {

		final ParseTreeWalker walker = new ParseTreeWalker();
		final SOQLListener listener = new SOQLListener();
		walker.walk(listener, tree);

		final CompiledStatement soqlStatement = new CompiledStatement();
		soqlStatement.entities = listener.getEntities();
		soqlStatement.whereExpression = listener.getWhereExpression();
		soqlStatement.selectClause = listener.getSelectClause();

		return soqlStatement;
	}

	@Test
	public void test1() throws Exception {

		String s = "SELECT r, a, a.getId() FROM Resource r, Activity a";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);
		
		// System.out.println(compiledStatement);

		final Map<String, Object> inputObjects = new HashMap<>();
		inputObjects.put("r", new MockObject());
		inputObjects.put("a", new MockObject());

		final Map<String, Object> queryParameter = new HashMap<>();

		final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(3, result.size());

	}

	/**
	 * positive test of a comparison expression in a WHERE clause 
	 */
	@Test
	public void test2() throws Exception {

		String s = "SELECT r FROM Resource r WHERE r.getName() = \"testName\"";

		final ParseTree tree = parseString(s);

		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();
		inputObjects.put("r", new MockObject());

		final Map<String, Object> queryParameter = new HashMap<>();

		final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
	}

	/**
	 * negative test of a comparison expression in a WHERE clause 
	 */
	@Test
	public void test3() throws Exception {

		String s = "SELECT r FROM Resource r WHERE r.getName() = \"testNameDiffering\"";

		final ParseTree tree = parseString(s);

		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();
		inputObjects.put("r", new MockObject());

		final Map<String, Object> queryParameter = new HashMap<>();

		final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertTrue(result.isEmpty());

	}

	/**
	 * positive test of a AND combined comparison expressions in WHERE clause 
	 */
	@Test
	public void test4() throws Exception {

		String s = "SELECT r, a FROM Resource r, Activity a WHERE r.getType() = a.getType() AND r.getName() = \"testName\"";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();
		inputObjects.put("r", new MockObject());
		inputObjects.put("a", new MockObject());

		final Map<String, Object> parameter = new HashMap<>();

		final List<Object> result = compiledStatement.evaluate(inputObjects, parameter);

		assertEquals(2, result.size());
	}

	/**
	 * positive test of a comparison expression with query parameter
	 */
	@Test
	public void test5() throws Exception {

		String s = "SELECT a FROM Activity a WHERE a.getName() = :outer_var";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("outer_var", "testName");

		final Map<String, Object> inputObjects = new HashMap<>();
		inputObjects.put("a", new MockObject());
		
		final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
	}

	/**
	 * positive test of a method call in comparison expression of a WHERE clause 
	 */
	@Test
	public void test6() throws Exception {

		String s = "SELECT a FROM Activity a WHERE a.getNumber() < :parameter";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();
		inputObjects.put("a", new MockObject());

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("parameter", 1234);

		final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
	}

	/**
	 * negative test of a method call in comparison expression of a WHERE clause 
	 */
	@Test
	public void test8() throws Exception {

		String s = "SELECT a FROM Activity a WHERE a.getNumber() > :parameter";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();
		inputObjects.put("a", new MockObject());

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("parameter", 122);

		final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(0, result.size());
	}

	/**
	 * negative test of a method call in SELECT statement
	 */
	@Test
	public void test9() throws Exception {

		String s = "SELECT a.getParameter(:param_1) FROM Activity a";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();

		final MockObject a = new MockObject();
		
		String matchingKey = "pid";
		a.putParameter(matchingKey, new MockParameter());

		inputObjects.put("a", a);

		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);

		List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
		
		String nonMatchingKey = "noPid";
		
		queryParameter = new HashMap<>();
		queryParameter.put("param_1", nonMatchingKey);

		result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertTrue(result.isEmpty());
	}
	
	/**
	 * positive test of a method call with multiple method arguments 
	 */
	@Test
	public void test10() throws Exception {

		String s = "SELECT a.getParameter(:param_1, :param_2) FROM Activity a";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();

		final MockObject a = new MockObject();
		
		String matchingKey = "pid";
		a.putParameter(matchingKey, new MockParameter());

		inputObjects.put("a", a);

		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);
		queryParameter.put("param_2", matchingKey);

		List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
		
		String nonMatchingKey = "noPid";
		
		queryParameter = new HashMap<>();
		queryParameter.put("param_1", nonMatchingKey);
		queryParameter.put("param_2", "");

		result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertTrue(result.isEmpty());
	}
	
	/**
	 * positive test of a chained method call in SELECT clause 
	 */
	@Test
	public void test11() throws Exception {

		String s = "SELECT a.getParameter(:param_1).getType() FROM Activity a";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();

		final MockObject a = new MockObject();
		
		String matchingKey = "pid";
		a.putParameter(matchingKey, new MockParameter());

		inputObjects.put("a", a);

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);

		List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
		
		assertEquals("testType", result.get(0));
	}
	
	/**
	 * positive test of a chained method call in WHERE clause 
	 */
	@Test
	public void test12() throws Exception {

		String s = "SELECT a FROM Activity a WHERE a.getParameter(:param_1).getType() = \"testType\"";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();

		final MockObject a = new MockObject();
		
		String matchingKey = "pid";
		a.putParameter(matchingKey, new MockParameter());

		inputObjects.put("a", a);

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);

		List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
	
	}
	
	/**
	 * negative test of a chained method call in WHERE clause 
	 */
	@Test
	public void test14() throws Exception {

		String s = "SELECT a FROM Activity a WHERE a.getParameter(:param_1).getType() = :param_2";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);
		
		final Map<String, Object> inputObjects = new HashMap<>();

		final MockObject a = new MockObject();
		
		final String matchingKey = "pid";
		a.putParameter(matchingKey, new MockParameter());

		inputObjects.put("a", a);

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);
		queryParameter.put("param_2", "notMatchingString");

		List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(0, result.size());
	}
	
	/**
	 * test an where expression with a NOT inversion
	 */
	@Test
	public void test15() throws Exception {

		String s = "SELECT a FROM Activity a WHERE NOT a.getParameter(:param_1).getType() = :param_2";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();

		final MockObject a = new MockObject();
		
		final String matchingKey = "pid";
		a.putParameter(matchingKey, new MockParameter());

		inputObjects.put("a", a);

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);
		queryParameter.put("param_2", "notMatchingString");

		List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
	}
	
	/**
	 * test an where expression with a NOT inversion
	 */
	@Test
	public void test16() throws Exception {

		String s = "SELECT a FROM Activity a WHERE a.getParameter(:param_1).getType() = :param_2";

		final ParseTree tree = parseString(s);
		final CompiledStatement compiledStatement = compile(tree);

		final Map<String, Object> inputObjects = new HashMap<>();

		final MockObject a = new MockObject();
		
		final String matchingKey = "pid";
		a.putParameter(matchingKey, new MockParameter());

		inputObjects.put("a", a);

		final Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("param_1", matchingKey);
		queryParameter.put("param_2", "testType"); 

		List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

		assertEquals(1, result.size());
	}

}
