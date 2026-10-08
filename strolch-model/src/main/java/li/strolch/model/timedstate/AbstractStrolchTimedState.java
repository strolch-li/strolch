/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.*;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.exception.StrolchException;
import li.strolch.utils.helper.StringHelper;

import java.text.MessageFormat;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

/**
 * Wrapper for a {@link IntegerTimedState}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("rawtypes")
public abstract class AbstractStrolchTimedState<T extends IValue> extends AbstractStrolchElement
		implements StrolchTimedState<T> {

	protected String id;
	protected String name;
	protected boolean readOnly;
	protected boolean hidden = false;
	protected int index;
	protected String interpretation = INTERPRETATION_NONE;
	protected String uom = UOM_NONE;

	protected Resource parent;
	protected ITimedState<T> state;

	protected AbstractStrolchTimedState() {
		this.state = new TimedState<>();
	}

	protected AbstractStrolchTimedState(String id, String name) {
		super(id, name);
		this.state = new TimedState<>();
	}

	@Override
	public String getId() {
		return this.id;
	}

	@Override
	public void setId(String id) {
		assertNotReadonly();
		id = trimOrEmpty(id).intern();
		if (StringHelper.isEmpty(id)) {
			String msg = "The id may never be empty for {0}";
			msg = MessageFormat.format(msg, getClass().getSimpleName());
			throw new StrolchException(msg);
		}
		this.id = id;
	}

	@Override
	public String getName() {
		return this.name;
	}

	@Override
	public void setName(String name) {
		assertNotReadonly();
		name = trimOrEmpty(name).intern();
		if (StringHelper.isEmpty(name)) {
			String msg = "The name may never be empty for {0} {1}";
			msg = MessageFormat.format(msg, getClass().getSimpleName(), getLocator());
			throw new StrolchException(msg);
		}
		this.name = name;
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

	@Override
	protected void fillClone(AbstractStrolchElement clone) {
		@SuppressWarnings("unchecked") AbstractStrolchTimedState<T> cloneT = (AbstractStrolchTimedState<T>) clone;
		cloneT.id = this.id;
		cloneT.name = this.name;
		cloneT.hidden = this.hidden;
		cloneT.index = this.index;
		cloneT.interpretation = this.interpretation;
		cloneT.uom = this.uom;
		cloneT.state = this.state.getCopy();
	}

	@Override
	public boolean isReadOnly() {
		return this.readOnly;
	}

	@Override
	public void setReadOnly() {
		this.state.setReadonly();
		this.readOnly = true;
	}

	@Override
	public void clear() {
		assertNotReadonly();
		this.state.getTimeEvolution().clear();
	}

	@Override
	public String toString() {

		return getClass().getSimpleName()
				+ " [id="
				+ this.id
				+ ", name="
				+ this.name
				+ ", valueNow="
				+ this.state.getStateAt(System.currentTimeMillis())
				+ "]";
	}
}
