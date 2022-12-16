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

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

import li.strolch.model.*;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.utils.helper.StringHelper;

/**
 * Wrapper for a {@link IntegerTimedState}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("rawtypes")
public abstract class AbstractStrolchTimedState<T extends IValue> extends AbstractStrolchElement
		implements StrolchTimedState<T> {

	protected boolean hidden = false;
	protected int index;
	protected String interpretation = INTERPRETATION_NONE;
	protected String uom = UOM_NONE;

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
	public void setId(String id) {
		super.setId(trimOrEmpty(id).intern());
	}

	@Override
	public void setName(String name) {
		super.setName(trimOrEmpty(name).intern());
	}

	@Override
	public boolean isHidden() {
		return this.hidden;
	}

	@Override
	public void setHidden(boolean hidden) {
		assertNotReadonly();
		this.hidden = hidden;
	}

	@Override
	public String getInterpretation() {
		return this.interpretation;
	}

	@Override
	public void setInterpretation(String interpretation) {
		assertNotReadonly();
		if (StringHelper.isEmpty(interpretation)) {
			this.interpretation = INTERPRETATION_NONE;
		} else {
			this.interpretation = interpretation;
		}
	}

	@Override
	public boolean isInterpretationDefined() {
		return !INTERPRETATION_NONE.equals(this.interpretation);
	}

	@Override
	public boolean isInterpretationEmpty() {
		return INTERPRETATION_NONE.equals(this.interpretation);
	}

	@Override
	public String getUom() {
		return this.uom;
	}

	@Override
	public void setUom(String uom) {
		assertNotReadonly();
		if (StringHelper.isEmpty(uom)) {
			this.uom = UOM_NONE;
		} else {
			this.uom = uom;
		}
	}

	@Override
	public boolean isUomDefined() {
		return !UOM_NONE.equals(this.uom);
	}

	@Override
	public boolean isUomEmpty() {
		return UOM_NONE.equals(this.uom);
	}

	@Override
	public void setIndex(int index) {
		assertNotReadonly();
		this.index = index;
	}

	@Override
	public int getIndex() {
		return this.index;
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
	public <U extends IValueChange<T>> void applyChange(U change, boolean compact) {
		this.state.applyChange(change, compact);
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
		assertNotReadonly();
		this.parent = parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return this.parent;
	}

	@Override
	public boolean isRootElement() {
		return false;
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
		lb.append(Tags.STATE);
		lb.append(this.id);
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		if (this.parent != null)
			this.parent.fillLocator(lb);
		fillLocator(lb);
		return lb.build();
	}

	protected void fillClone(AbstractStrolchTimedState<T> clone) {
		super.fillClone(clone);
		clone.hidden = this.hidden;
		clone.index = this.index;
		clone.interpretation = this.interpretation;
		clone.uom = this.uom;
		clone.state = this.state.getCopy();
	}

	@Override
	public void setReadOnly() {
		this.state.setReadonly();
		super.setReadOnly();
	}

	@Override
	public void clear() {
		assertNotReadonly();
		this.state.getTimeEvolution().clear();
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
