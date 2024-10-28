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

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.timedstate.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ParameterVisitor<U> extends StrolchElementVisitor<U> {

	@Override
	default U visitActivity(Activity activity) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + activity.getClass());
	}

	@Override
	default U visitOrder(Order order) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + order.getClass());
	}

	@Override
	default U visitResource(Resource resource) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + resource.getClass());
	}

	@Override
	default U visitAction(Action action) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + action.getClass());
	}

	@Override
	default U visitBooleanState(BooleanTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	default U visitFloatState(FloatTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	default U visitFloatListState(FloatListTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	default U visitIntegerListState(IntegerListTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	default U visitIntegerState(IntegerTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	default U visitLongState(LongTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	default U visitStringState(StringSetTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	default U visitParameterBag(ParameterBag bag) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + bag.getClass());
	}
}
