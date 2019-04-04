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
import li.strolch.model.parameter.*;
import li.strolch.model.timedstate.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchElementVisitor<U> extends StrolchVisitor {

	U visitOrder(Order order);

	U visitResource(Resource resource);

	U visitActivity(Activity activity);

	U visitAction(Action action);

	U visitBooleanParam(BooleanParameter param);

	U visitDateParam(DateParameter param);

	U visitDurationParam(DurationParameter param);

	U visitFloatParam(FloatParameter param);

	U visitIntegerParam(IntegerParameter param);

	U visitLongParam(LongParameter param);

	U visitStringParam(StringParameter param);

	U visitStringListParam(StringListParameter param);

	U visitIntegerListParam(IntegerListParameter param);

	U visitFloatListParam(FloatListParameter param);

	U visitLongListParam(LongListParameter param);

	U visitBooleanState(BooleanTimedState state);

	U visitFloatState(FloatTimedState state);

	U visitLongState(LongTimedState state);

	U visitFloatListState(FloatListTimedState state);

	U visitIntegerState(IntegerTimedState state);

	U visitStringState(StringSetTimedState state);

	U visitParameterBag(ParameterBag bag);
}
