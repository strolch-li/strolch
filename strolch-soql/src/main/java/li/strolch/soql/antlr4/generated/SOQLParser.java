// Generated from SOQL.g4 by ANTLR 4.7
package li.strolch.soql.antlr4.generated;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.ParserATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;

@SuppressWarnings({ "all", "warnings", "unchecked", "unused", "cast" })
public class SOQLParser extends Parser {
	static {
		RuntimeMetaData.checkVersion("4.7", RuntimeMetaData.VERSION);
	}

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache = new PredictionContextCache();
	public static final int T__0 = 1, T__1 = 2, T__2 = 3, T__3 = 4, T__4 = 5, T__5 = 6, T__6 = 7, T__7 = 8, T__8 = 9, T__9 = 10, T__10 = 11, T__11 = 12, T__12 = 13, T__13 = 14, T__14 = 15, T__15 = 16, T__16 = 17, T__17 = 18, T__18 = 19, T__19 = 20, T__20 = 21, T__21 = 22, T__22 = 23, T__23 = 24, IDENTIFICATION_VARIABLE = 25, STRING_VARIABLE = 26, WS = 27;
	public static final int RULE_select_statement = 0, RULE_select_clause = 1, RULE_select_expression = 2, RULE_chained_method_expression = 3, RULE_method_expression = 4, RULE_method_name = 5, RULE_method_argument = 6, RULE_from_clause = 7, RULE_entity_declaration = 8, RULE_class_declaration = 9, RULE_object_declaration = 10, RULE_where_clause = 11, RULE_or_expression = 12, RULE_and_expression = 13, RULE_expression_term = 14, RULE_comparison_expression = 15, RULE_comparison_operator = 16, RULE_var_reference = 17, RULE_value_declaration = 18, RULE_orderby_clause = 19, RULE_orderby_item = 20;
	public static final String[] ruleNames = { "select_statement",
			"select_clause",
			"select_expression",
			"chained_method_expression",
			"method_expression",
			"method_name",
			"method_argument",
			"from_clause",
			"entity_declaration",
			"class_declaration",
			"object_declaration",
			"where_clause",
			"or_expression",
			"and_expression",
			"expression_term",
			"comparison_expression",
			"comparison_operator",
			"var_reference",
			"value_declaration",
			"orderby_clause",
			"orderby_item" };

	private static final String[] _LITERAL_NAMES = { null,
			"'SELECT'",
			"'DISTINCT'",
			"','",
			"'.'",
			"'()'",
			"'('",
			"')'",
			"'FROM'",
			"'WHERE'",
			"'OR'",
			"'AND'",
			"'NOT'",
			"'='",
			"'>'",
			"'>='",
			"'<'",
			"'<='",
			"'<>'",
			"':'",
			"'\"'",
			"'ORDER'",
			"'BY'",
			"'ASC'",
			"'DESC'" };
	private static final String[] _SYMBOLIC_NAMES = { null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			null,
			"IDENTIFICATION_VARIABLE",
			"STRING_VARIABLE",
			"WS" };
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;

	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() {
		return "SOQL.g4";
	}

	@Override
	public String[] getRuleNames() {
		return ruleNames;
	}

	@Override
	public String getSerializedATN() {
		return _serializedATN;
	}

	@Override
	public ATN getATN() {
		return _ATN;
	}

	public SOQLParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
	}

	public static class Select_statementContext extends ParserRuleContext {
		public Select_clauseContext select_clause() {
			return getRuleContext(Select_clauseContext.class, 0);
		}

		public From_clauseContext from_clause() {
			return getRuleContext(From_clauseContext.class, 0);
		}

		public Where_clauseContext where_clause() {
			return getRuleContext(Where_clauseContext.class, 0);
		}

		public Orderby_clauseContext orderby_clause() {
			return getRuleContext(Orderby_clauseContext.class, 0);
		}

		public Select_statementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_select_statement;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterSelect_statement(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitSelect_statement(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitSelect_statement(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Select_statementContext select_statement() throws RecognitionException {
		Select_statementContext _localctx = new Select_statementContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_select_statement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(42);
				select_clause();
				setState(43);
				from_clause();
				setState(45);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la == T__8) {
					{
						setState(44);
						where_clause();
					}
				}

				setState(48);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la == T__20) {
					{
						setState(47);
						orderby_clause();
					}
				}

			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Select_clauseContext extends ParserRuleContext {
		public List<Select_expressionContext> select_expression() {
			return getRuleContexts(Select_expressionContext.class);
		}

		public Select_expressionContext select_expression(int i) {
			return getRuleContext(Select_expressionContext.class, i);
		}

		public Select_clauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_select_clause;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterSelect_clause(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitSelect_clause(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitSelect_clause(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Select_clauseContext select_clause() throws RecognitionException {
		Select_clauseContext _localctx = new Select_clauseContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_select_clause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(50);
				match(T__0);
				setState(52);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la == T__1) {
					{
						setState(51);
						match(T__1);
					}
				}

				setState(54);
				select_expression();
				setState(59);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la == T__2) {
					{
						{
							setState(55);
							match(T__2);
							setState(56);
							select_expression();
						}
					}
					setState(61);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Select_expressionContext extends ParserRuleContext {
		public Chained_method_expressionContext chained_method_expression() {
			return getRuleContext(Chained_method_expressionContext.class, 0);
		}

		public Object_declarationContext object_declaration() {
			return getRuleContext(Object_declarationContext.class, 0);
		}

		public Select_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_select_expression;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterSelect_expression(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitSelect_expression(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitSelect_expression(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Select_expressionContext select_expression() throws RecognitionException {
		Select_expressionContext _localctx = new Select_expressionContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_select_expression);
		try {
			setState(64);
			_errHandler.sync(this);
			switch (getInterpreter().adaptivePredict(_input, 4, _ctx)) {
			case 1:
				enterOuterAlt(_localctx, 1);
			{
				setState(62);
				chained_method_expression();
			}
			break;
			case 2:
				enterOuterAlt(_localctx, 2);
			{
				setState(63);
				object_declaration();
			}
			break;
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Chained_method_expressionContext extends ParserRuleContext {
		public TerminalNode IDENTIFICATION_VARIABLE() {
			return getToken(SOQLParser.IDENTIFICATION_VARIABLE, 0);
		}

		public List<Method_expressionContext> method_expression() {
			return getRuleContexts(Method_expressionContext.class);
		}

		public Method_expressionContext method_expression(int i) {
			return getRuleContext(Method_expressionContext.class, i);
		}

		public Chained_method_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_chained_method_expression;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterChained_method_expression(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitChained_method_expression(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitChained_method_expression(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Chained_method_expressionContext chained_method_expression() throws RecognitionException {
		Chained_method_expressionContext _localctx = new Chained_method_expressionContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_chained_method_expression);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(66);
				match(IDENTIFICATION_VARIABLE);
				setState(67);
				match(T__3);
				setState(68);
				method_expression();
				setState(73);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la == T__3) {
					{
						{
							setState(69);
							match(T__3);
							setState(70);
							method_expression();
						}
					}
					setState(75);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Method_expressionContext extends ParserRuleContext {
		public Method_nameContext method_name() {
			return getRuleContext(Method_nameContext.class, 0);
		}

		public Method_argumentContext method_argument() {
			return getRuleContext(Method_argumentContext.class, 0);
		}

		public Method_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_method_expression;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterMethod_expression(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitMethod_expression(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitMethod_expression(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Method_expressionContext method_expression() throws RecognitionException {
		Method_expressionContext _localctx = new Method_expressionContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_method_expression);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(76);
				method_name();
				setState(77);
				method_argument();
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Method_nameContext extends ParserRuleContext {
		public TerminalNode IDENTIFICATION_VARIABLE() {
			return getToken(SOQLParser.IDENTIFICATION_VARIABLE, 0);
		}

		public Method_nameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_method_name;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterMethod_name(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitMethod_name(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitMethod_name(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Method_nameContext method_name() throws RecognitionException {
		Method_nameContext _localctx = new Method_nameContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_method_name);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(79);
				match(IDENTIFICATION_VARIABLE);
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Method_argumentContext extends ParserRuleContext {
		public List<Var_referenceContext> var_reference() {
			return getRuleContexts(Var_referenceContext.class);
		}

		public Var_referenceContext var_reference(int i) {
			return getRuleContext(Var_referenceContext.class, i);
		}

		public Method_argumentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_method_argument;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterMethod_argument(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitMethod_argument(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitMethod_argument(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Method_argumentContext method_argument() throws RecognitionException {
		Method_argumentContext _localctx = new Method_argumentContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_method_argument);
		int _la;
		try {
			setState(93);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__4:
				enterOuterAlt(_localctx, 1);
			{
				setState(81);
				match(T__4);
			}
			break;
			case T__5:
				enterOuterAlt(_localctx, 2);
			{
				setState(82);
				match(T__5);
				setState(83);
				var_reference();
				setState(88);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la == T__2) {
					{
						{
							setState(84);
							match(T__2);
							setState(85);
							var_reference();
						}
					}
					setState(90);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(91);
				match(T__6);
			}
			break;
			default:
				throw new NoViableAltException(this);
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class From_clauseContext extends ParserRuleContext {
		public List<Entity_declarationContext> entity_declaration() {
			return getRuleContexts(Entity_declarationContext.class);
		}

		public Entity_declarationContext entity_declaration(int i) {
			return getRuleContext(Entity_declarationContext.class, i);
		}

		public From_clauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_from_clause;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterFrom_clause(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitFrom_clause(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitFrom_clause(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final From_clauseContext from_clause() throws RecognitionException {
		From_clauseContext _localctx = new From_clauseContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_from_clause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(95);
				match(T__7);
				setState(96);
				entity_declaration();
				setState(101);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la == T__2) {
					{
						{
							setState(97);
							match(T__2);
							setState(98);
							entity_declaration();
						}
					}
					setState(103);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Entity_declarationContext extends ParserRuleContext {
		public Class_declarationContext class_declaration() {
			return getRuleContext(Class_declarationContext.class, 0);
		}

		public Object_declarationContext object_declaration() {
			return getRuleContext(Object_declarationContext.class, 0);
		}

		public Entity_declarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_entity_declaration;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterEntity_declaration(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitEntity_declaration(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitEntity_declaration(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Entity_declarationContext entity_declaration() throws RecognitionException {
		Entity_declarationContext _localctx = new Entity_declarationContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_entity_declaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(104);
				class_declaration();
				setState(105);
				object_declaration();
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Class_declarationContext extends ParserRuleContext {
		public TerminalNode IDENTIFICATION_VARIABLE() {
			return getToken(SOQLParser.IDENTIFICATION_VARIABLE, 0);
		}

		public Class_declarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_class_declaration;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterClass_declaration(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitClass_declaration(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitClass_declaration(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Class_declarationContext class_declaration() throws RecognitionException {
		Class_declarationContext _localctx = new Class_declarationContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_class_declaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(107);
				match(IDENTIFICATION_VARIABLE);
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Object_declarationContext extends ParserRuleContext {
		public TerminalNode IDENTIFICATION_VARIABLE() {
			return getToken(SOQLParser.IDENTIFICATION_VARIABLE, 0);
		}

		public Object_declarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_object_declaration;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterObject_declaration(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitObject_declaration(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitObject_declaration(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Object_declarationContext object_declaration() throws RecognitionException {
		Object_declarationContext _localctx = new Object_declarationContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_object_declaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(109);
				match(IDENTIFICATION_VARIABLE);
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Where_clauseContext extends ParserRuleContext {
		public Or_expressionContext or_expression() {
			return getRuleContext(Or_expressionContext.class, 0);
		}

		public Where_clauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_where_clause;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterWhere_clause(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitWhere_clause(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitWhere_clause(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Where_clauseContext where_clause() throws RecognitionException {
		Where_clauseContext _localctx = new Where_clauseContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_where_clause);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(111);
				match(T__8);
				setState(112);
				or_expression();
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Or_expressionContext extends ParserRuleContext {
		public List<And_expressionContext> and_expression() {
			return getRuleContexts(And_expressionContext.class);
		}

		public And_expressionContext and_expression(int i) {
			return getRuleContext(And_expressionContext.class, i);
		}

		public Or_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_or_expression;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterOr_expression(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitOr_expression(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitOr_expression(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Or_expressionContext or_expression() throws RecognitionException {
		Or_expressionContext _localctx = new Or_expressionContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_or_expression);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				{
					setState(114);
					and_expression();
				}
				setState(119);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la == T__9) {
					{
						{
							setState(115);
							match(T__9);
							setState(116);
							and_expression();
						}
					}
					setState(121);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class And_expressionContext extends ParserRuleContext {
		public List<Expression_termContext> expression_term() {
			return getRuleContexts(Expression_termContext.class);
		}

		public Expression_termContext expression_term(int i) {
			return getRuleContext(Expression_termContext.class, i);
		}

		public And_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_and_expression;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterAnd_expression(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitAnd_expression(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitAnd_expression(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final And_expressionContext and_expression() throws RecognitionException {
		And_expressionContext _localctx = new And_expressionContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_and_expression);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				{
					setState(122);
					expression_term();
				}
				setState(127);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la == T__10) {
					{
						{
							setState(123);
							match(T__10);
							setState(124);
							expression_term();
						}
					}
					setState(129);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Expression_termContext extends ParserRuleContext {
		public Comparison_expressionContext comparison_expression() {
			return getRuleContext(Comparison_expressionContext.class, 0);
		}

		public Expression_termContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_expression_term;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterExpression_term(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitExpression_term(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitExpression_term(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Expression_termContext expression_term() throws RecognitionException {
		Expression_termContext _localctx = new Expression_termContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_expression_term);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(131);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la == T__11) {
					{
						setState(130);
						match(T__11);
					}
				}

				setState(133);
				comparison_expression();
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Comparison_expressionContext extends ParserRuleContext {
		public List<Chained_method_expressionContext> chained_method_expression() {
			return getRuleContexts(Chained_method_expressionContext.class);
		}

		public Chained_method_expressionContext chained_method_expression(int i) {
			return getRuleContext(Chained_method_expressionContext.class, i);
		}

		public Comparison_operatorContext comparison_operator() {
			return getRuleContext(Comparison_operatorContext.class, 0);
		}

		public Value_declarationContext value_declaration() {
			return getRuleContext(Value_declarationContext.class, 0);
		}

		public Var_referenceContext var_reference() {
			return getRuleContext(Var_referenceContext.class, 0);
		}

		public Comparison_expressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_comparison_expression;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterComparison_expression(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitComparison_expression(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitComparison_expression(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Comparison_expressionContext comparison_expression() throws RecognitionException {
		Comparison_expressionContext _localctx = new Comparison_expressionContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_comparison_expression);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(135);
				chained_method_expression();
				setState(136);
				comparison_operator();
				setState(140);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case T__19: {
					setState(137);
					value_declaration();
				}
				break;
				case T__18: {
					setState(138);
					var_reference();
				}
				break;
				case IDENTIFICATION_VARIABLE: {
					setState(139);
					chained_method_expression();
				}
				break;
				default:
					throw new NoViableAltException(this);
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Comparison_operatorContext extends ParserRuleContext {
		public Comparison_operatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_comparison_operator;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterComparison_operator(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitComparison_operator(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitComparison_operator(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Comparison_operatorContext comparison_operator() throws RecognitionException {
		Comparison_operatorContext _localctx = new Comparison_operatorContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_comparison_operator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(142);
				_la = _input.LA(1);
				if (!((((_la) & ~0x3f) == 0 &&
						((1L << _la) & ((1L << T__12) | (1L << T__13) | (1L << T__14) | (1L << T__15) | (1L << T__16)
								| (1L << T__17))) != 0))) {
					_errHandler.recoverInline(this);
				} else {
					if (_input.LA(1) == Token.EOF)
						matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Var_referenceContext extends ParserRuleContext {
		public TerminalNode IDENTIFICATION_VARIABLE() {
			return getToken(SOQLParser.IDENTIFICATION_VARIABLE, 0);
		}

		public Var_referenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_var_reference;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterVar_reference(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitVar_reference(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitVar_reference(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Var_referenceContext var_reference() throws RecognitionException {
		Var_referenceContext _localctx = new Var_referenceContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_var_reference);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(144);
				match(T__18);
				setState(145);
				match(IDENTIFICATION_VARIABLE);
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Value_declarationContext extends ParserRuleContext {
		public TerminalNode IDENTIFICATION_VARIABLE() {
			return getToken(SOQLParser.IDENTIFICATION_VARIABLE, 0);
		}

		public Value_declarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_value_declaration;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterValue_declaration(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitValue_declaration(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitValue_declaration(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Value_declarationContext value_declaration() throws RecognitionException {
		Value_declarationContext _localctx = new Value_declarationContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_value_declaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(147);
				match(T__19);
				setState(148);
				match(IDENTIFICATION_VARIABLE);
				setState(149);
				match(T__19);
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Orderby_clauseContext extends ParserRuleContext {
		public List<Orderby_itemContext> orderby_item() {
			return getRuleContexts(Orderby_itemContext.class);
		}

		public Orderby_itemContext orderby_item(int i) {
			return getRuleContext(Orderby_itemContext.class, i);
		}

		public Orderby_clauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_orderby_clause;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterOrderby_clause(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitOrderby_clause(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitOrderby_clause(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Orderby_clauseContext orderby_clause() throws RecognitionException {
		Orderby_clauseContext _localctx = new Orderby_clauseContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_orderby_clause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(151);
				match(T__20);
				setState(152);
				match(T__21);
				setState(153);
				orderby_item();
				setState(158);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la == T__2) {
					{
						{
							setState(154);
							match(T__2);
							setState(155);
							orderby_item();
						}
					}
					setState(160);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Orderby_itemContext extends ParserRuleContext {
		public Chained_method_expressionContext chained_method_expression() {
			return getRuleContext(Chained_method_expressionContext.class, 0);
		}

		public Orderby_itemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}

		@Override
		public int getRuleIndex() {
			return RULE_orderby_item;
		}

		@Override
		public void enterRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).enterOrderby_item(this);
		}

		@Override
		public void exitRule(ParseTreeListener listener) {
			if (listener instanceof SOQLListener)
				((SOQLListener) listener).exitOrderby_item(this);
		}

		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if (visitor instanceof SOQLVisitor)
				return ((SOQLVisitor<? extends T>) visitor).visitOrderby_item(this);
			else
				return visitor.visitChildren(this);
		}
	}

	public final Orderby_itemContext orderby_item() throws RecognitionException {
		Orderby_itemContext _localctx = new Orderby_itemContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_orderby_item);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
				setState(161);
				chained_method_expression();
				setState(163);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la == T__22 || _la == T__23) {
					{
						setState(162);
						_la = _input.LA(1);
						if (!(_la == T__22 || _la == T__23)) {
							_errHandler.recoverInline(this);
						} else {
							if (_input.LA(1) == Token.EOF)
								matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
					}
				}

			}
		} catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		} finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN = "\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\35\u00a8\4\2\t\2"
			+ "\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"
			+ "\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"
			+ "\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\3\2\3\2\3\2\5\2\60\n\2\3\2\5"
			+ "\2\63\n\2\3\3\3\3\5\3\67\n\3\3\3\3\3\3\3\7\3<\n\3\f\3\16\3?\13\3\3\4\3"
			+ "\4\5\4C\n\4\3\5\3\5\3\5\3\5\3\5\7\5J\n\5\f\5\16\5M\13\5\3\6\3\6\3\6\3"
			+ "\7\3\7\3\b\3\b\3\b\3\b\3\b\7\bY\n\b\f\b\16\b\\\13\b\3\b\3\b\5\b`\n\b\3"
			+ "\t\3\t\3\t\3\t\7\tf\n\t\f\t\16\ti\13\t\3\n\3\n\3\n\3\13\3\13\3\f\3\f\3"
			+ "\r\3\r\3\r\3\16\3\16\3\16\7\16x\n\16\f\16\16\16{\13\16\3\17\3\17\3\17"
			+ "\7\17\u0080\n\17\f\17\16\17\u0083\13\17\3\20\5\20\u0086\n\20\3\20\3\20"
			+ "\3\21\3\21\3\21\3\21\3\21\5\21\u008f\n\21\3\22\3\22\3\23\3\23\3\23\3\24"
			+ "\3\24\3\24\3\24\3\25\3\25\3\25\3\25\3\25\7\25\u009f\n\25\f\25\16\25\u00a2"
			+ "\13\25\3\26\3\26\5\26\u00a6\n\26\3\26\2\2\27\2\4\6\b\n\f\16\20\22\24\26"
			+ "\30\32\34\36 \"$&(*\2\4\3\2\17\24\3\2\31\32\2\u00a2\2,\3\2\2\2\4\64\3"
			+ "\2\2\2\6B\3\2\2\2\bD\3\2\2\2\nN\3\2\2\2\fQ\3\2\2\2\16_\3\2\2\2\20a\3\2"
			+ "\2\2\22j\3\2\2\2\24m\3\2\2\2\26o\3\2\2\2\30q\3\2\2\2\32t\3\2\2\2\34|\3"
			+ "\2\2\2\36\u0085\3\2\2\2 \u0089\3\2\2\2\"\u0090\3\2\2\2$\u0092\3\2\2\2"
			+ "&\u0095\3\2\2\2(\u0099\3\2\2\2*\u00a3\3\2\2\2,-\5\4\3\2-/\5\20\t\2.\60"
			+ "\5\30\r\2/.\3\2\2\2/\60\3\2\2\2\60\62\3\2\2\2\61\63\5(\25\2\62\61\3\2"
			+ "\2\2\62\63\3\2\2\2\63\3\3\2\2\2\64\66\7\3\2\2\65\67\7\4\2\2\66\65\3\2"
			+ "\2\2\66\67\3\2\2\2\678\3\2\2\28=\5\6\4\29:\7\5\2\2:<\5\6\4\2;9\3\2\2\2"
			+ "<?\3\2\2\2=;\3\2\2\2=>\3\2\2\2>\5\3\2\2\2?=\3\2\2\2@C\5\b\5\2AC\5\26\f"
			+ "\2B@\3\2\2\2BA\3\2\2\2C\7\3\2\2\2DE\7\33\2\2EF\7\6\2\2FK\5\n\6\2GH\7\6"
			+ "\2\2HJ\5\n\6\2IG\3\2\2\2JM\3\2\2\2KI\3\2\2\2KL\3\2\2\2L\t\3\2\2\2MK\3"
			+ "\2\2\2NO\5\f\7\2OP\5\16\b\2P\13\3\2\2\2QR\7\33\2\2R\r\3\2\2\2S`\7\7\2"
			+ "\2TU\7\b\2\2UZ\5$\23\2VW\7\5\2\2WY\5$\23\2XV\3\2\2\2Y\\\3\2\2\2ZX\3\2"
			+ "\2\2Z[\3\2\2\2[]\3\2\2\2\\Z\3\2\2\2]^\7\t\2\2^`\3\2\2\2_S\3\2\2\2_T\3"
			+ "\2\2\2`\17\3\2\2\2ab\7\n\2\2bg\5\22\n\2cd\7\5\2\2df\5\22\n\2ec\3\2\2\2"
			+ "fi\3\2\2\2ge\3\2\2\2gh\3\2\2\2h\21\3\2\2\2ig\3\2\2\2jk\5\24\13\2kl\5\26"
			+ "\f\2l\23\3\2\2\2mn\7\33\2\2n\25\3\2\2\2op\7\33\2\2p\27\3\2\2\2qr\7\13"
			+ "\2\2rs\5\32\16\2s\31\3\2\2\2ty\5\34\17\2uv\7\f\2\2vx\5\34\17\2wu\3\2\2"
			+ "\2x{\3\2\2\2yw\3\2\2\2yz\3\2\2\2z\33\3\2\2\2{y\3\2\2\2|\u0081\5\36\20"
			+ "\2}~\7\r\2\2~\u0080\5\36\20\2\177}\3\2\2\2\u0080\u0083\3\2\2\2\u0081\177"
			+ "\3\2\2\2\u0081\u0082\3\2\2\2\u0082\35\3\2\2\2\u0083\u0081\3\2\2\2\u0084"
			+ "\u0086\7\16\2\2\u0085\u0084\3\2\2\2\u0085\u0086\3\2\2\2\u0086\u0087\3"
			+ "\2\2\2\u0087\u0088\5 \21\2\u0088\37\3\2\2\2\u0089\u008a\5\b\5\2\u008a"
			+ "\u008e\5\"\22\2\u008b\u008f\5&\24\2\u008c\u008f\5$\23\2\u008d\u008f\5"
			+ "\b\5\2\u008e\u008b\3\2\2\2\u008e\u008c\3\2\2\2\u008e\u008d\3\2\2\2\u008f"
			+ "!\3\2\2\2\u0090\u0091\t\2\2\2\u0091#\3\2\2\2\u0092\u0093\7\25\2\2\u0093"
			+ "\u0094\7\33\2\2\u0094%\3\2\2\2\u0095\u0096\7\26\2\2\u0096\u0097\7\33\2"
			+ "\2\u0097\u0098\7\26\2\2\u0098\'\3\2\2\2\u0099\u009a\7\27\2\2\u009a\u009b"
			+ "\7\30\2\2\u009b\u00a0\5*\26\2\u009c\u009d\7\5\2\2\u009d\u009f\5*\26\2"
			+ "\u009e\u009c\3\2\2\2\u009f\u00a2\3\2\2\2\u00a0\u009e\3\2\2\2\u00a0\u00a1"
			+ "\3\2\2\2\u00a1)\3\2\2\2\u00a2\u00a0\3\2\2\2\u00a3\u00a5\5\b\5\2\u00a4"
			+ "\u00a6\t\3\2\2\u00a5\u00a4\3\2\2\2\u00a5\u00a6\3\2\2\2\u00a6+\3\2\2\2"
			+ "\21/\62\66=BKZ_gy\u0081\u0085\u008e\u00a0\u00a5";
	public static final ATN _ATN = new ATNDeserializer().deserialize(_serializedATN.toCharArray());

	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}