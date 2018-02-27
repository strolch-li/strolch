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
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchElementVisitor<U> extends StrolchVisitor {

	public U visitOrder(Order order);

	public U visitResource(Resource resource);

	public U visitActivity(Activity activity);

	public U visitAction(Action action);

	public U visitBooleanParam(BooleanParameter param);

	public U visitDateParam(DateParameter param);

	public U visitDurationParam(DurationParameter param);

	public U visitFloatParam(FloatParameter param);

	public U visitIntegerParam(IntegerParameter param);

	public U visitLongParam(LongParameter param);

	public U visitStringParam(StringParameter param);

	public U visitStringListParam(StringListParameter param);

	public U visitIntegerListParam(IntegerListParameter param);

	public U visitFloatListParam(FloatListParameter param);

	public U visitLongListParam(LongListParameter param);

	public U visitBooleanState(BooleanTimedState state);

	public U visitFloatState(FloatTimedState state);

	public U visitIntegerState(IntegerTimedState state);

	public U visitStringState(StringSetTimedState state);

	public U visitParameterBag(ParameterBag bag);
}
