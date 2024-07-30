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

import java.util.Objects;

import li.strolch.model.StrolchValueType;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.visitor.StrolchElementVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IntegerTimedState extends AbstractStrolchTimedState<IntegerValue> {

	public IntegerTimedState() {
		super();
	}

	public IntegerTimedState(String id, String name) {
		super(id, name);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null || getClass() != obj.getClass())
			return false;
		IntegerTimedState o = (IntegerTimedState) obj;
		return this.parent == o.parent && this.id.equals(o.id);
	}

	@Override
	public int hashCode() {
		return Objects.hash(parent, id);
	}

	@Override
	public void setStateFromStringAt(Long time, String value) {
		assertNotReadonly();
		getTimeEvolution().setValueAt(time, new IntegerValue(value));
	}

	@Override
	public String getType() {
		return StrolchValueType.INTEGER.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.INTEGER;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitIntegerState(this);
	}

	@Override
	public IntegerTimedState getClone() {
		IntegerTimedState clone = new IntegerTimedState();
		fillClone(clone);
		return clone;
	}
}
