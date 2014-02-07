/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.visitor;

import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.ListParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SetParameterValueVisitor implements ParameterVisitor {

	private String value;

	public void setValue(Parameter<?> parameter, String value) {
		this.value = value;
		try {
			parameter.accept(this);
		} finally {
			this.value = null;
		}
	}

	@Override
	public <T> T visitParam(Parameter<?> param) {
		throw new UnsupportedOperationException("Not implemented on " + param.getClass());
	}

	@Override
	public <T> T visitListParam(ListParameter<?> param) {
		throw new UnsupportedOperationException("Not implemented on " + param.getClass());
	}

	@Override
	public <T> T visitBooleanParam(BooleanParameter param) {
		param.setValue(BooleanParameter.parseFromString(this.value));
		return null;
	}

	@Override
	public <T> T visitDateParam(DateParameter param) {
		param.setValue(DateParameter.parseFromString(this.value));
		return null;
	}

	@Override
	public <T> T visitFloatParam(FloatParameter param) {
		param.setValue(FloatParameter.parseFromString(this.value));
		return null;
	}

	@Override
	public <T> T visitIntegerParam(IntegerParameter param) {
		param.setValue(IntegerParameter.parseFromString(this.value));
		return null;
	}

	@Override
	public <T> T visitLongParam(LongParameter param) {
		param.setValue(LongParameter.parseFromString(this.value));
		return null;
	}

	@Override
	public <T> T visitStringParam(StringParameter param) {
		param.setValue(this.value);
		return null;
	}

	@Override
	public <T> T visitStringListParam(StringListParameter param) {
		param.setValue(StringListParameter.parseFromString(this.value));
		return null;
	}

}
