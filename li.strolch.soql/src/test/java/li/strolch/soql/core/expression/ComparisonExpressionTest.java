package li.strolch.soql.core.expression;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import li.strolch.soql.core.expresssion.ComparisonExpression;
import li.strolch.soql.core.expresssion.ParameterReference;
import li.strolch.soql.core.expresssion.ValueDeclaration;

public class ComparisonExpressionTest {

	@Test
	public void testStringMatch() {

		ValueDeclaration vd_1 = new ValueDeclaration();
		vd_1.valueAsString = "Wes Montgomery";

		ValueDeclaration vd_2 = new ValueDeclaration();
		vd_2.valueAsString = "Wes Montgomery";

		ComparisonExpression ce = new ComparisonExpression();
		ce.addOperand(vd_1);
		ce.addOperand(vd_2);
		ce.setOperator("=");

		assertTrue(ce.evaluate(null, null));
	}

	@Test
	public void testStringNoMatch() {

		ValueDeclaration vd_1 = new ValueDeclaration();
		vd_1.valueAsString = "Wes Montgomery";

		ValueDeclaration vd_2 = new ValueDeclaration();
		vd_2.valueAsString = "Charlie Parker";

		ComparisonExpression ce = new ComparisonExpression();
		ce.addOperand(vd_1);
		ce.addOperand(vd_2);
		ce.setOperator("<>");

		assertTrue(ce.evaluate(null, null));
	}
	
	@Test
	public void testNumericMatch() {

		ParameterReference vd_1 = new ParameterReference();
		vd_1.varName = "a";

		ParameterReference vd_2 = new ParameterReference();
		vd_2.varName = "b";

		ComparisonExpression ce = new ComparisonExpression();
		ce.addOperand(vd_1);
		ce.addOperand(vd_2);
		ce.setOperator("=");
		
		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 123L);		

		assertTrue(ce.evaluate(null, queryParameter));
		
		queryParameter = new HashMap<>();
		queryParameter.put("a", 1L);
		queryParameter.put("b", 123L);		

		assertFalse(ce.evaluate(null, queryParameter));
	}
	
	@Test
	public void testNumericLess() {

		ParameterReference vd_1 = new ParameterReference();
		vd_1.varName = "a";

		ParameterReference vd_2 = new ParameterReference();
		vd_2.varName = "b";

		ComparisonExpression ce = new ComparisonExpression();
		ce.addOperand(vd_1);
		ce.addOperand(vd_2);
		ce.setOperator("<");
		
		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("a", 1L);
		queryParameter.put("b", 123L);		

		assertTrue(ce.evaluate(null, queryParameter));
		
		queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 123L);		

		assertFalse(ce.evaluate(null, queryParameter));
	}
	
	@Test
	public void testNumericMore() {

		ParameterReference vd_1 = new ParameterReference();
		vd_1.varName = "a";

		ParameterReference vd_2 = new ParameterReference();
		vd_2.varName = "b";

		ComparisonExpression ce = new ComparisonExpression();
		ce.addOperand(vd_1);
		ce.addOperand(vd_2);
		ce.setOperator(">");
		
		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 1L);		

		assertTrue(ce.evaluate(null, queryParameter));
		
		queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 123L);		

		assertFalse(ce.evaluate(null, queryParameter));
	}
	
	@Test
	public void testNumericLessEuals() {

		ParameterReference vd_1 = new ParameterReference();
		vd_1.varName = "a";

		ParameterReference vd_2 = new ParameterReference();
		vd_2.varName = "b";

		ComparisonExpression ce = new ComparisonExpression();
		ce.addOperand(vd_1);
		ce.addOperand(vd_2);
		ce.setOperator("<=");
		
		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("a", 1L);
		queryParameter.put("b", 123L);		

		assertTrue(ce.evaluate(null, queryParameter));
		
		queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 123L);		

		assertTrue(ce.evaluate(null, queryParameter));
		
		queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 1L);		

		assertFalse(ce.evaluate(null, queryParameter));
	}
	
	@Test
	public void testNumericMoreEuals() {

		ParameterReference vd_1 = new ParameterReference();
		vd_1.varName = "a";

		ParameterReference vd_2 = new ParameterReference();
		vd_2.varName = "b";

		ComparisonExpression ce = new ComparisonExpression();
		ce.addOperand(vd_1);
		ce.addOperand(vd_2);
		ce.setOperator(">=");
		
		Map<String, Object> queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 1L);		

		assertTrue(ce.evaluate(null, queryParameter));
		
		queryParameter = new HashMap<>();
		queryParameter.put("a", 123L);
		queryParameter.put("b", 123L);		

		assertTrue(ce.evaluate(null, queryParameter));
		
		queryParameter = new HashMap<>();
		queryParameter.put("a", 1L);
		queryParameter.put("b", 123L);		

		assertFalse(ce.evaluate(null, queryParameter));
	}
	

}
