package li.strolch.soql.core;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchElementQuery;

/**
 * @author msmock
 */
public class QueryTest extends BaseTest {

    /**
     * @param entities
     * @return Map of queries for the entities defining the objects returned
     */
    public Map<String, StrolchElementQuery> resolveEntities(final Map<String, String> entities) {

        final Map<String, StrolchElementQuery> result = new HashMap<>();

        final Set<String> keys = entities.keySet();
        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext(); ) {

            final String key = iterator.next();
            final String clazzKey = entities.get(key);

            // System.out.println("key = " + key + ", class = " + clazzKey);

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
            }
        }
        return result;
    }

    @Test
    public void testQuery() throws Exception {

        final String s = "SELECT a, r, o FROM Activity a, Resource r, Order o";

        final ParseTree tree = parseString(s);
        final CompiledStatement compiledStatement = compile(tree);
        final Map<String, String> entities = compiledStatement.entities;

        final Map<String, StrolchElementQuery> queries = resolveEntities(entities);

        assertEquals(ActivityQuery.class, queries.get("a").getClass());
        assertEquals(ResourceQuery.class, queries.get("r").getClass());
        assertEquals(OrderQuery.class, queries.get("o").getClass());
    }

}
