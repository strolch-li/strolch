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
package li.strolch.model.timedstate;

import li.strolch.model.StrolchValueType;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.visitor.StrolchElementVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FloatTimedState extends AbstractStrolchTimedState<FloatValue> {

	public FloatTimedState() {
		super();
	}

	public FloatTimedState(String id, String name) {
		super(id, name);
	}

	@Override
	public void setStateFromStringAt(Long time, String value) {
		assertNotReadonly();
		getTimeEvolution().setValueAt(time, new FloatValue(value));
	}

	@Override
	public String getType() {
		return StrolchValueType.FLOAT.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.FLOAT;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitFloatState(this);
	}

	@Override
	public FloatTimedState getClone() {
		FloatTimedState clone = new FloatTimedState();
		fillClone(clone);
		return clone;
	}
}
