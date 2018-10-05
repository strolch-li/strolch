// Generated from SOQL.g4 by ANTLR 4.7
package li.strolch.soql.antlr4.generated;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by {@link SOQLParser}.
 */
public interface SOQLListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link SOQLParser#select_statement}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterSelect_statement(SOQLParser.Select_statementContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#select_statement}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitSelect_statement(SOQLParser.Select_statementContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#select_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterSelect_clause(SOQLParser.Select_clauseContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#select_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitSelect_clause(SOQLParser.Select_clauseContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#select_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterSelect_expression(SOQLParser.Select_expressionContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#select_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitSelect_expression(SOQLParser.Select_expressionContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#chained_method_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterChained_method_expression(SOQLParser.Chained_method_expressionContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#chained_method_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitChained_method_expression(SOQLParser.Chained_method_expressionContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#method_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterMethod_expression(SOQLParser.Method_expressionContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#method_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitMethod_expression(SOQLParser.Method_expressionContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#method_name}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterMethod_name(SOQLParser.Method_nameContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#method_name}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitMethod_name(SOQLParser.Method_nameContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#method_argument}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterMethod_argument(SOQLParser.Method_argumentContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#method_argument}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitMethod_argument(SOQLParser.Method_argumentContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#from_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterFrom_clause(SOQLParser.From_clauseContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#from_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitFrom_clause(SOQLParser.From_clauseContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#entity_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterEntity_declaration(SOQLParser.Entity_declarationContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#entity_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitEntity_declaration(SOQLParser.Entity_declarationContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#class_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterClass_declaration(SOQLParser.Class_declarationContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#class_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitClass_declaration(SOQLParser.Class_declarationContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#object_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterObject_declaration(SOQLParser.Object_declarationContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#object_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitObject_declaration(SOQLParser.Object_declarationContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#where_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterWhere_clause(SOQLParser.Where_clauseContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#where_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitWhere_clause(SOQLParser.Where_clauseContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#or_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterOr_expression(SOQLParser.Or_expressionContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#or_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitOr_expression(SOQLParser.Or_expressionContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#and_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterAnd_expression(SOQLParser.And_expressionContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#and_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitAnd_expression(SOQLParser.And_expressionContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#expression_term}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterExpression_term(SOQLParser.Expression_termContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#expression_term}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitExpression_term(SOQLParser.Expression_termContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#comparison_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterComparison_expression(SOQLParser.Comparison_expressionContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#comparison_expression}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitComparison_expression(SOQLParser.Comparison_expressionContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#comparison_operator}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterComparison_operator(SOQLParser.Comparison_operatorContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#comparison_operator}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitComparison_operator(SOQLParser.Comparison_operatorContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#var_reference}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterVar_reference(SOQLParser.Var_referenceContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#var_reference}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitVar_reference(SOQLParser.Var_referenceContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#value_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterValue_declaration(SOQLParser.Value_declarationContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#value_declaration}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitValue_declaration(SOQLParser.Value_declarationContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#orderby_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterOrderby_clause(SOQLParser.Orderby_clauseContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#orderby_clause}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitOrderby_clause(SOQLParser.Orderby_clauseContext ctx);

	/**
	 * Enter a parse tree produced by {@link SOQLParser#orderby_item}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void enterOrderby_item(SOQLParser.Orderby_itemContext ctx);

	/**
	 * Exit a parse tree produced by {@link SOQLParser#orderby_item}.
	 *
	 * @param ctx
	 * 		the parse tree
	 */
	void exitOrderby_item(SOQLParser.Orderby_itemContext ctx);
}