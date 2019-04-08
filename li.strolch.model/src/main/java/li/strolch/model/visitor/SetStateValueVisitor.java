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

import java.util.List;

import li.strolch.model.timedstate.*;
import li.strolch.model.timevalue.impl.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SetStateValueVisitor implements TimedStateVisitor<Void> {

	private long time;
	private Object value;

	public SetStateValueVisitor(long time, Object value) {
		this.time = time;
		this.value = value;
	}

	@Override
	public Void visitBooleanState(BooleanTimedState state) {
		state.getTimeEvolution().setValueAt(this.time, new BooleanValue((Boolean) value));
		return null;
	}

	@Override
	public Void visitFloatState(FloatTimedState state) {
		state.getTimeEvolution().setValueAt(this.time, new FloatValue((Double) value));
		return null;
	}

	@Override
	public Void visitLongState(LongTimedState state) {
		state.getTimeEvolution().setValueAt(this.time, new LongValue((Long) value));
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Void visitFloatListState(FloatListTimedState state) {
		state.getTimeEvolution().setValueAt(this.time, new FloatListValue((List<Double>) value));
		return null;
	}

	@Override
	public Void visitIntegerState(IntegerTimedState state) {
		state.getTimeEvolution().setValueAt(this.time, new IntegerValue((Integer) value));
		return null;
	}

	@Override
	public Void visitStringState(StringSetTimedState state) {
		state.getTimeEvolution().setValueAt(this.time, new StringSetValue((String) value));
		return null;
	}
}
