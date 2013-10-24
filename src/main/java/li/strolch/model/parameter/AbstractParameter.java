/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of li.strolch.model.
 *
 *  li.strolch.model is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  li.strolch.model is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with li.strolch.model.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.model.parameter;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.Tags;

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

	protected boolean hidden;
	protected String interpretation;
	protected String uom;

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
			this.interpretation = Parameter.INTERPRETATION_NONE;
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
			this.uom = Parameter.UOM_NONE;
		} else {
			this.uom = uom;
		}
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
	public Element toDom(Document doc) {
		Element element = doc.createElement(Tags.PARAMETER);
		fillElement(element);

		element.setAttribute(Tags.VALUE, getValueAsString());

		if (!this.interpretation.equals(Parameter.INTERPRETATION_NONE))
			element.setAttribute(Tags.INTERPRETATION, this.interpretation);
		if (!this.uom.equals(Parameter.UOM_NONE))
			element.setAttribute(Tags.UOM, this.uom);
		if (this.hidden)
			element.setAttribute(Tags.HIDDEN, Boolean.toString(this.hidden));

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
		String isHidden = element.getAttribute(Tags.HIDDEN);
		String uom = element.getAttribute(Tags.UOM);

		setInterpretation(interpretation);
		setUom(uom);

		if (isHidden == null) {
			setHidden(false);
		} else {
			if (isHidden.equalsIgnoreCase(Boolean.TRUE.toString())) {
				setHidden(true);
			} else if (isHidden.equalsIgnoreCase(Boolean.FALSE.toString())) {
				setHidden(false);
			} else {
				String msg = "Boolean string must be either {0} or {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, Boolean.TRUE.toString(), Boolean.FALSE.toString());
				throw new StrolchException(msg);
			}
		}
	}

	@Override
	protected void fillLocator(LocatorBuilder locatorBuilder) {
		locatorBuilder.append(Tags.PARAMETER).append(this.id);
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		this.parent.fillLocator(lb);
		this.fillLocator(lb);
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
