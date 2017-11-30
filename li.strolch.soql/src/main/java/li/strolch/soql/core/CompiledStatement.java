package li.strolch.soql.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.soql.core.expresssion.SelectClause;
import li.strolch.soql.core.expresssion.WhereExpression;

public class CompiledStatement {

	// the map of entities declared in the FROM clause with their nicknames as keys
	Map<String, String> entities = new HashMap<>();

	// the decision tree as defined in the WHERE clause
	WhereExpression whereExpression;

	// the expression evaluating to the selected objects according to the SELECT
	// clause
	public SelectClause selectClause;

	/**
	 * 
	 * @return List of objects resulting from running the compiled code
	 */
	public List<Object> evaluate(Map<String, Object> inputObjects, Map<String, Object> injectedObjects) {

		boolean evaluateWhereExpression = true;
		if (whereExpression != null) {
			evaluateWhereExpression = whereExpression.evaluate(inputObjects, injectedObjects);
		}

		if (evaluateWhereExpression) {
			return selectClause.evaluate(inputObjects, injectedObjects);
		}

		return new ArrayList<>();
	}

	@Override
	public String toString() {
		return "CompiledStatement [entities=" + entities + ", whereExpression=" + whereExpression + ", selectClause="
				+ selectClause + "]";
	}

}
