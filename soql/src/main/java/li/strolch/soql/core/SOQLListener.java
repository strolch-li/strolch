package li.strolch.soql.core;

import java.util.HashMap;
import java.util.Map;

import li.strolch.soql.antlr4.generated.SOQLBaseListener;
import li.strolch.soql.antlr4.generated.SOQLParser.*;
import li.strolch.soql.core.expresssion.*;

/**
 * @author msmock
 */
public class SOQLListener extends SOQLBaseListener {

	// the map of entities declared in the FROM clause with their nicknames as keys
	private final Map<String, String> entities = new HashMap<>();

	// the definition of the result set from the SELECT clause
	private SelectClause selectClause;

	// the decision tree as defined in the WHERE clause
	private WhereExpression whereExpression;

	// pointer to the current parent during parsing
	private IExpression pointer = null;

	// used to track the entity class Name during parsing
	private String currentEntity;

	public Map<String, String> getEntities() {
		return entities;
	}

	public WhereExpression getWhereExpression() {
		return whereExpression;
	}

	public SelectClause getSelectClause() {
		return selectClause;
	}

	@Override
	public void enterSelect_clause(Select_clauseContext ctx) {
		pointer = new SelectClause();
	}

	@Override
	public void exitSelect_clause(Select_clauseContext ctx) {
		this.selectClause = (SelectClause) pointer;
		pointer = null;
	}

	@Override
	public void enterSelect_expression(Select_expressionContext ctx) {
		SelectExpression selectExpression = new SelectExpression();
		((SelectClause) pointer).addExpression(selectExpression);
		pointer = selectExpression;
	}

	@Override
	public void exitSelect_expression(Select_expressionContext ctx) {
		pointer = pointer.getParent();
	}

	@Override
	public void enterWhere_clause(Where_clauseContext ctx) {
		pointer = new WhereExpression();
	}

	@Override
	public void exitWhere_clause(Where_clauseContext ctx) {
		this.whereExpression = (WhereExpression) pointer;
		pointer = null;
	}

	/**
	 * extract the class declaration in the FROM clause. For example, in 'FROM Resource res' the class declaration is
	 * 'Resource'.
	 */
	@Override
	public void exitClass_declaration(Class_declarationContext ctx) {
		currentEntity = ctx.getText();
	}

	/**
	 * extract the object declaration in the FROM clause, which defines the lookup key for the class declaration. For
	 * example, in 'FROM Resource res' the object declaration is 'res'.
	 */
	@Override
	public void exitObject_declaration(Object_declarationContext ctx) {
		if (pointer instanceof SelectExpression) {
			ObjectDeclaration expr = new ObjectDeclaration();
			expr.key = ctx.getText();
			((SelectExpression) pointer).addExpression(expr);
		} else if (pointer == null) {
			String key = ctx.getText();
			entities.put(key, currentEntity);
			currentEntity = null;
		}
	}

	/**
	 * one or expression can have multiple and
	 */
	@Override
	public void enterOr_expression(Or_expressionContext ctx) {
		OrExpression orExpression = new OrExpression();
		((WhereExpression) pointer).setOrExpression(orExpression);
		pointer = orExpression;
	}

	/**
	 * one or expression can have multiple and
	 */
	@Override
	public void enterAnd_expression(And_expressionContext ctx) {
		AndExpression andExpression = new AndExpression();
		((OrExpression) pointer).addAndExpression(andExpression);
		pointer = andExpression;
	}

	@Override
	public void enterExpression_term(Expression_termContext ctx) {

		ExpressionTerm expressionTerm = new ExpressionTerm();
		((AndExpression) pointer).addExpressionTerm(expressionTerm);
		pointer = expressionTerm;

		if (ctx.children.get(0).getText().equals("NOT")) {
			expressionTerm.setNot(true);
		}
	}

	@Override
	public void enterComparison_expression(Comparison_expressionContext ctx) {
		ComparisonExpression comparisonExpresion = new ComparisonExpression();
		comparisonExpresion.setOperator(ctx.children.get(1).getText());
		((ExpressionTerm) pointer).setComparison(comparisonExpresion);
		pointer = comparisonExpresion;
	}

	@Override
	public void enterChained_method_expression(Chained_method_expressionContext ctx) {

		final ChainedMethodExpression chainedMethodExpression = new ChainedMethodExpression();
		chainedMethodExpression.setObjectKey(ctx.children.get(0).getText());

		if (pointer instanceof SelectExpression) {
			((SelectExpression) pointer).addExpression(chainedMethodExpression);
		} else if (pointer instanceof ComparisonExpression) {
			((ComparisonExpression) pointer).addOperand(chainedMethodExpression);
		}
		pointer = chainedMethodExpression;
	}

	@Override
	public void enterMethod_expression(Method_expressionContext ctx) {
		MethodExpression methodExpression = new MethodExpression();
		((ChainedMethodExpression) pointer).addMethodExpression(methodExpression);
		pointer = methodExpression;
	}

	@Override
	public void exitMethod_expression(Method_expressionContext ctx) {
		pointer = pointer.getParent();
	}

	@Override
	public void enterMethod_name(Method_nameContext ctx) {
		if (pointer instanceof MethodExpression) {
			((MethodExpression) pointer).setMethodName(ctx.getText());
		}
	}

	@Override
	public void enterMethod_argument(Method_argumentContext ctx) {
		MethodArgumentDeclaration methodArguments = new MethodArgumentDeclaration();
		((MethodExpression) pointer).setMethodArguments(methodArguments);
		pointer = methodArguments;
	}

	@Override
	public void exitMethod_argument(Method_argumentContext ctx) {
		pointer = pointer.getParent();
	}

	@Override
	public void exitChained_method_expression(Chained_method_expressionContext ctx) {
		pointer = pointer.getParent();
	}

	@Override
	public void enterValue_declaration(Value_declarationContext ctx) {
		ValueDeclaration valueDeclaration = new ValueDeclaration();
		valueDeclaration.valueAsString = ctx.children.get(1).getText();
		((ComparisonExpression) pointer).addOperand(valueDeclaration);
	}

	@Override
	public void enterVar_reference(Var_referenceContext ctx) {
		ParameterReference parameterReference = new ParameterReference();
		parameterReference.varName = ctx.children.get(1).getText();
		if (pointer instanceof ComparisonExpression) {
			((ComparisonExpression) pointer).addOperand(parameterReference);
		} else if (pointer instanceof MethodArgumentDeclaration) {
			((MethodArgumentDeclaration) pointer).addParameterReference(parameterReference);
		}
	}

	@Override
	public void exitComparison_expression(Comparison_expressionContext ctx) {
		pointer = pointer.getParent();
	}

	@Override
	public void exitExpression_term(Expression_termContext ctx) {
		pointer = pointer.getParent();
	}

	@Override
	public void exitAnd_expression(And_expressionContext ctx) {
		pointer = pointer.getParent();
	}

	@Override
	public void exitOr_expression(Or_expressionContext ctx) {
		pointer = pointer.getParent();
	}

}
