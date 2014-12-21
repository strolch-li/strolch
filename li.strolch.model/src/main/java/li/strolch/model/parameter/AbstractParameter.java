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
import li.strolch.model.Tags;
import li.strolch.model.visitor.ParameterVisitor;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.StringHelper;

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
	public Element toDom(Document doc) {
		Element element = doc.createElement(Tags.PARAMETER);
		fillElement(element);

		element.setAttribute(Tags.VALUE, getValueAsString());

		if (!this.interpretation.equals(INTERPRETATION_NONE)) {
			element.setAttribute(Tags.INTERPRETATION, this.interpretation);
		}
		if (!this.uom.equals(UOM_NONE)) {
			element.setAttribute(Tags.UOM, this.uom);
		}
		if (this.hidden) {
			element.setAttribute(Tags.HIDDEN, Boolean.toString(this.hidden));
		}
		if (this.index != 0) {
			element.setAttribute(Tags.INDEX, Integer.toString(this.index));
		}

		return element;
	}

	@Override
	public void fromDom(Element element) {

		super.fromDom(element);

		String typeS = element.getAttribute(Tags.TYPE);
		if (StringHelper.isEmpty(typeS)) {
			String msg = "Type must be set on element with id {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.id);
			throw new StrolchException(msg);
		} else if (!typeS.equals(getType())) {
			String msg = "{0} must have type {1}, not: {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getClass().getSimpleName(), getType(), typeS);
			throw new StrolchException(msg);
		}

		String interpretation = element.getAttribute(Tags.INTERPRETATION);
		String hidden = element.getAttribute(Tags.HIDDEN);
		String uom = element.getAttribute(Tags.UOM);
		String index = element.getAttribute(Tags.INDEX);

		setInterpretation(interpretation);
		setUom(uom);

		if (StringHelper.isEmpty(index)) {
			this.index = 0;
		} else {
			this.index = Integer.valueOf(index);
		}

		if (StringHelper.isEmpty(hidden)) {
			setHidden(false);
		} else {
			if (hidden.equalsIgnoreCase(Boolean.TRUE.toString())) {
				setHidden(true);
			} else if (hidden.equalsIgnoreCase(Boolean.FALSE.toString())) {
				setHidden(false);
			} else {
				String msg = "Boolean string must be either {0} or {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, Boolean.TRUE.toString(), Boolean.FALSE.toString());
				throw new StrolchException(msg);
			}
		}
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
		lb.append(this.id);
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
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
			String msg = "{0} Parameter {1} may not have a null value!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getType(), getId());
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
}
