package li.strolch.soql.core;

import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author msmock
 */
public class StatementTest extends BaseTest{

    @Test
    public void test0() throws Exception {

        String s = "SELECT r FROM Resource r";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        // System.out.println(compiledStatement);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("r", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();

        final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());
    }

    @Test
    public void test1() throws Exception {

        String s = "SELECT r, a, a.getId() FROM Resource r, Activity a";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        // System.out.println(compiledStatement);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("r", getTestResource("testId"));
        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();

        final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(3, result.size());
    }

    /**
     * positive test of a comparison expression in a WHERE clause
     */
    @Test
    public void test2() throws Exception {

        String s = "SELECT r FROM Resource r WHERE r.getId() = \"testId\"";

        final ParseTree tree = parseString(s);

        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("r", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();

        final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());
    }

    /**
     * negative test of a comparison expression in a WHERE clause
     */
    @Test
    public void test3() throws Exception {

        String s = "SELECT r FROM Resource r WHERE r.getId() = \"testIdNoMatch\"";

        final ParseTree tree = parseString(s);

        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("r", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();

        final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertTrue(result.isEmpty());
    }

    /**
     * positive test of a AND combined comparison expressions in WHERE clause
     */
    @Test
    public void test4() throws Exception {

        String s = "SELECT r, a FROM Resource r, Activity a WHERE r.getId() = a.getId() AND r.getId() = \"testId\"";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("r", getTestResource("testId"));
        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> parameter = new HashMap<>();

        final List<Object> result = compiledStatement.evaluate(inputObjects, parameter);

        assertEquals(2, result.size());
    }

    /**
     * positive test of a comparison expression with query parameter
     */
    @Test
    public void test5() throws Exception {

        String s = "SELECT a FROM Activity a WHERE a.getId() = :outer_var";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("outer_var", "testId");

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("a", getTestResource("testId"));

        final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());
    }

    /**
     * positive test of a method call in comparison expression of a WHERE clause
     */
    @Test
    public void test6() throws Exception {

        String s = "SELECT a FROM Activity a WHERE a.getParameter(:p_1, :p_2).getValue() < :parameter";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "testId");
        queryParameter.put("parameter", 101d);

        final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());
    }

    /**
     * negative test of a method call in comparison expression of a WHERE clause
     */
    @Test
    public void test8() throws Exception {

        String s = "SELECT a FROM Activity a WHERE a.getParameter(:p_1, :p_2).getValue() > :parameter";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "testId");
        queryParameter.put("parameter", 101d);

        final List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(0, result.size());
    }

    /**
     * negative test of a method call in SELECT statement
     */
    @Test
    public void test9() throws Exception {

        String s = "SELECT a.getParameterBag(:param_1) FROM Resource a";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();

        inputObjects.put("a", getTestResource("testId"));

        Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("param_1", "testBag");

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

        String s = "SELECT a.getParameter(:param_1,:param_2) FROM Activity a";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();

        inputObjects.put("a", getTestResource("testId"));

        Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("param_1", "testBag");
        queryParameter.put("param_2", "testId");

        List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());

        queryParameter = new HashMap<>();
        queryParameter.put("param_1", "nex");
        queryParameter.put("param_2", "testId");

        result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertTrue(result.isEmpty());
    }

    /**
     * positive test of a chained method call in SELECT clause
     */
    @Test
    public void test11() throws Exception {

        String s = "SELECT a.getParameterBag(:param_1).getId() FROM Activity a";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();

        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("param_1", "testBag");

        List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());

        assertEquals("testBag", result.get(0));
    }

    /**
     * positive test of a chained method call in WHERE clause
     */
    @Test
    public void test12() throws Exception {

        String s = "SELECT a FROM Activity a WHERE a.getParameter(:p_1, :p_2).getType() = \"Float\"";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();

        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "testId");

        List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());
    }

    /**
     * negative test of a chained method call in WHERE clause
     */
    @Test
    public void test14() throws Exception {

        String s = "SELECT a FROM Activity a WHERE a.getParameter(:p_1, :p_2).getType() = :p_3";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();

        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "testId");
        queryParameter.put("p_3", "String");

        List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(0, result.size());
    }

    /**
     * test an where expression with a NOT inversion
     */
    @Test
    public void test15() throws Exception {

        String s = "SELECT a FROM Activity a WHERE NOT a.getParameter(:p_1, :p_2).getType() = :p_3";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();

        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "testId");
        queryParameter.put("p_3", "String");

        List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(1, result.size());
    }

    /**
     * test null pointer in chained method calls, when the first call returns null
     */
    @Test
    public void test16() throws Exception {

        String s = "SELECT a FROM Activity a WHERE a.getParameter(:p_1, :p_2).getType() = :p_3";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);

        final Map<String, Object> inputObjects = new HashMap<>();
        inputObjects.put("a", getTestResource("testId"));

        final Map<String, Object> queryParameter = new HashMap<>();
        queryParameter.put("p_1", "testBag");
        queryParameter.put("p_2", "bullshitId");
        queryParameter.put("p_3", "String");

        List<Object> result = compiledStatement.evaluate(inputObjects, queryParameter);

        assertEquals(0, result.size());
    }

}
