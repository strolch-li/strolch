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
package li.strolch.model.parameter;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.visitor.ParameterVisitor;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 * @param <T>
 */
public abstract class AbstractParameter<T> extends AbstractStrolchElement implements Parameter<T> {

	private static final long serialVersionUID = 0L;

	protected boolean hidden = false;
	protected int index;
	protected String interpretation = INTERPRETATION_NONE;
	protected String uom = UOM_NONE;

	protected ParameterizedElement parent;

	/**
	 * Empty constructor
	 */
	protected AbstractParameter() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * @param name
	 */
	public AbstractParameter(String id, String name) {
		super(id, name);
	}

	@Override
	public boolean isHidden() {
		return this.hidden;
	}

	@Override
	public void setHidden(boolean hidden) {
		this.hidden = hidden;
	}

	@Override
	public String getInterpretation() {
		return this.interpretation;
	}

	@Override
	public void setInterpretation(String interpretation) {
		if (StringHelper.isEmpty(interpretation)) {
			this.interpretation = INTERPRETATION_NONE;
		} else {
			this.interpretation = interpretation;
		}
	}

	@Override
	public String getUom() {
		return this.uom;
	}

	@Override
	public void setUom(String uom) {
		if (StringHelper.isEmpty(uom)) {
			this.uom = UOM_NONE;
		} else {
			this.uom = uom;
		}
	}

	@Override
	public void setIndex(int index) {
		this.index = index;
	}

	@Override
	public int getIndex() {
		return this.index;
	}

	@Override
	public ParameterizedElement getParent() {
		return this.parent;
	}

	@Override
	public void setParent(ParameterizedElement parent) {
		this.parent = parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return this.parent.getRootElement();
	}

	@Override
	public boolean isRootElement() {
		return false;
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
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

	/**
	 * Validates that the value is legal. This is the case when it is not null in this implementation
	 *
	 * @param value
	 *            the value to check for this parameter instance
	 *
	 * @throws StrolchException
	 *             if the value is null
	 */
	protected void validateValue(T value) throws StrolchException {
		if (value == null) {
			String msg = "Can not set null value on Parameter {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getLocator());
			throw new StrolchException(msg);
		}
	}

	/**
	 * Fills the {@link Parameter} clone with the id, name, hidden, interpretation and uom
	 *
	 * @param clone
	 */
	protected void fillClone(Parameter<?> clone) {
		super.fillClone(clone);

		clone.setHidden(this.hidden);
		clone.setInterpretation(this.interpretation);
		clone.setUom(this.uom);
		clone.setIndex(this.index);
	}

	@Override
	public <U> U accept(ParameterVisitor visitor) {
		return visitor.visitParam(this);
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
		builder.append(", value=");
		builder.append(getValueAsString());
		builder.append("]");

		return builder.toString();
	}

	@Override
	public abstract int compareTo(Parameter<?> otherParam);
}
