/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package li.strolch.soql.core;

import li.strolch.soql.core.expresssion.ComparisonExpression;
import li.strolch.soql.core.expresssion.ParameterReference;
import li.strolch.soql.core.expresssion.ValueDeclaration;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author msmock
 */
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
	public void testNumericLessEquals() {

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
