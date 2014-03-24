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

import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("rawtypes")
public abstract class AbstractStrolchTimedState<T extends IValue> extends AbstractStrolchElement implements
		StrolchTimedState<T> {

	private static final long serialVersionUID = 1L;

	protected Resource parent;
	protected ITimedState<T> state;

	public AbstractStrolchTimedState() {
		this.state = new TimedState<>();
	}

	public AbstractStrolchTimedState(String id, String name) {
		super(id, name);
		this.state = new TimedState<>();
	}

	@Override
	public ITimeValue<T> getNextMatch(Long time, T value) {
		return this.state.getNextMatch(time, value);
	}

	@Override
	public ITimeValue<T> getPreviousMatch(Long time, T value) {
		return this.state.getPreviousMatch(time, value);
	}

	@Override
	public <U extends IValueChange<T>> void applyChange(U change) {
		this.state.applyChange(change);
	}

	@Override
	public ITimeValue<T> getStateAt(Long time) {
		return this.state.getStateAt(time);
	}

	@Override
	public ITimeVariable<T> getTimeEvolution() {
		return this.state.getTimeEvolution();
	}

	@Override
	public StrolchElement getParent() {
		return this.parent;
	}

	@Override
	public void setParent(Resource parent) {
		this.parent = parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return this.parent;
	}

	@Override
	protected void fillLocator(LocatorBuilder locatorBuilder) {
		locatorBuilder.append(Tags.TIMED_STATE);
		locatorBuilder.append(getId());
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		this.parent.fillLocator(lb);
		fillLocator(lb);
		return lb.build();
	}

	protected void fillClone(AbstractStrolchTimedState<T> clone) {
		super.fillClone(clone);
		clone.state = this.state.getCopy();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {

		StringBuilder builder = new StringBuilder();

		builder.append(getClass().getSimpleName());
		builder.append(" [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", valueNow=");
		builder.append(this.state.getStateAt(System.currentTimeMillis()));
		builder.append("]");

		return builder.toString();
	}
}
