/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * Abstract class for all expressions which evaluate to Boolean result
 *
 * @author msmock
 */
public abstract class AbstractBooleanExpression implements IBooleanExpression {

	IExpression parent;

	@Override
	public abstract boolean evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter);

	@Override
	public void setParent(IExpression e) {
		parent = e;
	}

	@Override
	public IExpression getParent() {
		return parent;
	}

}
