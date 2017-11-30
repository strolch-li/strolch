// Generated from SOQL.g4 by ANTLR 4.7
package li.strolch.soql.antlr4.generated;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link SOQLParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface SOQLVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link SOQLParser#select_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelect_statement(SOQLParser.Select_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#select_clause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelect_clause(SOQLParser.Select_clauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#select_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelect_expression(SOQLParser.Select_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#chained_method_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitChained_method_expression(SOQLParser.Chained_method_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#method_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod_expression(SOQLParser.Method_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#method_name}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod_name(SOQLParser.Method_nameContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#method_argument}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod_argument(SOQLParser.Method_argumentContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#from_clause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFrom_clause(SOQLParser.From_clauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#entity_declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEntity_declaration(SOQLParser.Entity_declarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#class_declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClass_declaration(SOQLParser.Class_declarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#object_declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitObject_declaration(SOQLParser.Object_declarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#where_clause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhere_clause(SOQLParser.Where_clauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#or_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOr_expression(SOQLParser.Or_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#and_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnd_expression(SOQLParser.And_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#expression_term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpression_term(SOQLParser.Expression_termContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#comparison_expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComparison_expression(SOQLParser.Comparison_expressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#comparison_operator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComparison_operator(SOQLParser.Comparison_operatorContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#var_reference}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVar_reference(SOQLParser.Var_referenceContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#value_declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValue_declaration(SOQLParser.Value_declarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#orderby_clause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrderby_clause(SOQLParser.Orderby_clauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SOQLParser#orderby_item}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrderby_item(SOQLParser.Orderby_itemContext ctx);
}