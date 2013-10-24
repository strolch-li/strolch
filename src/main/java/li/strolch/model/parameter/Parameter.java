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

import li.strolch.model.Order;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface Parameter<T> extends StrolchElement {

	/**
	 * This interpretation value indicates that the {@link Parameter} has no defined interpretation
	 */
	public static final String INTERPRETATION_NONE = "None"; //$NON-NLS-1$

	/**
	 * This uom value indicates that the {@link Parameter} has no defined uom
	 */
	public static final String UOM_NONE = "None"; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to a {@link Resource}
	 */
	public static final String INTERPRETATION_RESOURCE_REF = "Resource-Reference"; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to a {@link Order}
	 */
	public static final String INTERPRETATION_ORDER_REF = "Order-Reference"; //$NON-NLS-1$

	/**
	 * the value of the parameter as string
	 * 
	 * @return String
	 */
	public String getValueAsString();

	/**
	 * the value of the parameter
	 * 
	 * @return
	 */
	public T getValue();

	/**
	 * the value of the parameter
	 * 
	 * @param value
	 */
	public void setValue(T value);

	/**
	 * get the hidden attribute
	 * 
	 * @return
	 */
	public boolean isHidden();

	/**
	 * set the hidden attribute
	 * 
	 * @param hidden
	 */
	public void setHidden(boolean hidden);

	/**
	 * Get the UOM of this {@link Parameter}
	 * 
	 * @return
	 */
	public String getUom();

	/**
	 * Set the UOM of this {@link Parameter}
	 * 
	 * @param uom
	 */
	public void setUom(String uom);

	/**
	 * The {@link ParameterizedElement} parent to which this {@link Parameter} belongs
	 * 
	 * @return
	 */
	public ParameterizedElement getParent();

	/**
	 * Sets the parent for this {@link Parameter}
	 */
	public void setParent(ParameterizedElement parent);

	/**
	 * Returns the interpretation of this {@link Parameter}. The interpretation semantic describes what the value of
	 * this {@link Parameter} means. Currently there are three definitions, but any String value can be used:
	 * <ul>
	 * <li>{@link Parameter#INTERPRETATION_NONE}</li>
	 * <li>{@link Parameter#INTERPRETATION_ORDER_REF}</li>
	 * <li>{@link Parameter#INTERPRETATION_RESOURCE_REF}</li>
	 * </ul>
	 * 
	 * @return string value
	 */
	public String getInterpretation();

	/**
	 * Set the interpretation of this {@link Parameter}. The interpretation semantic describes what the value of this
	 * {@link Parameter} means. Currently there are three definitions, but any String value can be used:
	 * <ul>
	 * <li>{@link Parameter#INTERPRETATION_NONE}</li>
	 * <li>{@link Parameter#INTERPRETATION_ORDER_REF}</li>
	 * <li>{@link Parameter#INTERPRETATION_RESOURCE_REF}</li>
	 * </ul>
	 * 
	 * @param interpretation
	 */
	public void setInterpretation(String interpretation);

	@Override
	public int hashCode();

	@Override
	public boolean equals(Object obj);
}
