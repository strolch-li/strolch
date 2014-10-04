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

import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.visitor.TimedStateVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("rawtypes")
public interface StrolchTimedState<T extends IValue> extends StrolchElement {

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
	 * Returns the index of this {@link Parameter}. This can be used to sort the parameters in a UI
	 * 
	 * @return the index of this {@link Parameter}. This can be used to sort the parameters in a UI
	 */
	public int getIndex();

	/**
	 * Set the index of this {@link Parameter}. This can be used to sort the parameters in a UI
	 * 
	 * @param index
	 *            the index to set
	 */
	public void setIndex(int index);

	/**
	 * Returns the interpretation of this {@link Parameter}. The interpretation semantic describes what the value of
	 * this {@link Parameter} means. Currently there are three definitions, but any String value can be used:
	 * <ul>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_NONE}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_ORDER_REF}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}</li>
	 * </ul>
	 * 
	 * @return string value
	 */
	public String getInterpretation();

	/**
	 * Set the interpretation of this {@link Parameter}. The interpretation semantic describes what the value of this
	 * {@link Parameter} means. Currently there are three definitions, but any String value can be used:
	 * <ul>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_NONE}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_ORDER_REF}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}</li>
	 * </ul>
	 * 
	 * @param interpretation
	 */
	public void setInterpretation(String interpretation);

	public ITimeValue<T> getNextMatch(Long time, T value);

	public ITimeValue<T> getPreviousMatch(Long time, T value);

	public <U extends IValueChange<T>> void applyChange(U change);

	public ITimeValue<T> getStateAt(Long time);

	public ITimeVariable<T> getTimeEvolution();

	public void setParent(Resource aThis);

	@Override
	public StrolchTimedState<T> getClone();

	public <U> U accept(TimedStateVisitor visitor);
}
