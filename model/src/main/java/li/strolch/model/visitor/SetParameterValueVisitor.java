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

import java.util.Date;
import java.util.List;

import li.strolch.model.parameter.*;
import li.strolch.utils.time.PeriodDuration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SetParameterValueVisitor implements ParameterVisitor<Void> {

	private final Object value;

	public SetParameterValueVisitor(Object value) {
		this.value = value;
	}

	@Override
	public Void visitBooleanParam(BooleanParameter param) {
		param.setValue((Boolean) this.value);
		return null;
	}

	@Override
	public Void visitDateParam(DateParameter param) {
		param.setValue((Date) this.value);
		return null;
	}

	@Override
	public Void visitDurationParam(DurationParameter param) {
		param.setValue((PeriodDuration) this.value);
		return null;
	}

	@Override
	public Void visitFloatParam(FloatParameter param) {
		param.setValue((Double) this.value);
		return null;
	}

	@Override
	public Void visitIntegerParam(IntegerParameter param) {
		param.setValue((Integer) this.value);
		return null;
	}

	@Override
	public Void visitLongParam(LongParameter param) {
		param.setValue((Long) this.value);
		return null;
	}

	@Override
	public Void visitStringParam(StringParameter param) {
		param.setValue((String) this.value);
		return null;
	}

	@Override
	public Void visitTextParam(TextParameter param) {
		param.setValue((String) this.value);
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Void visitStringListParam(StringListParameter param) {
		param.setValue((List<String>) this.value);
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Void visitFloatListParam(FloatListParameter param) {
		param.setValue((List<Double>) this.value);
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Void visitIntegerListParam(IntegerListParameter param) {
		param.setValue((List<Integer>) this.value);
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Void visitLongListParam(LongListParameter param) {
		param.setValue((List<Long>) this.value);
		return null;
	}
}
