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

import li.strolch.model.ParameterBag;
import li.strolch.model.activity.Action;
import li.strolch.model.parameter.*;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchRootElementVisitor<U> extends StrolchElementVisitor<U> {

	@Override
	public default U visitAction(Action action) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + action.getClass());
	}

	@Override
	public default U visitBooleanParam(BooleanParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitDateParam(DateParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitDurationParam(DurationParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitFloatParam(FloatParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitIntegerParam(IntegerParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitLongParam(LongParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitStringParam(StringParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitStringListParam(StringListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitIntegerListParam(IntegerListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitFloatListParam(FloatListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitLongListParam(LongListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitBooleanState(BooleanTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitFloatState(FloatTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitIntegerState(IntegerTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitStringState(StringSetTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitParameterBag(ParameterBag bag) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + bag.getClass());
	}
}
