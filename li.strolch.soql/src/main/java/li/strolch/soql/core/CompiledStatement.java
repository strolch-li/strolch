package li.strolch.soql.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.soql.core.expresssion.SelectClause;
import li.strolch.soql.core.expresssion.WhereExpression;

/**
 * @author msmock
 */
public class CompiledStatement {

    // the map of entities declared in the FROM clause with their nicknames as keys
    Map<String, String> entities = new HashMap<>();

    // the decision tree as defined in the WHERE clause
    WhereExpression whereExpression;

    // the expression evaluating to the selected objects according to the SELECT clause
    public SelectClause selectClause;

    /**
     *
     * @param inputObjects
     * @param queryParameter
     *
     * @return List of objects resulting from running the compiled code
     */
    public List<Object> evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

        boolean evaluateWhereExpression = true;
        if (whereExpression != null)
            evaluateWhereExpression = whereExpression.evaluate(inputObjects, queryParameter);

        if (evaluateWhereExpression)
            return selectClause.evaluate(inputObjects, queryParameter);

        // else return empty list
        return new ArrayList<>();
    }

    @Override
    public String toString() {
        return "CompiledStatement [entities=" + entities + ", whereExpression=" + whereExpression + ", selectClause="
                + selectClause + "]";
    }

}
