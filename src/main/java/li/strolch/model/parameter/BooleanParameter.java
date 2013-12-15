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

import java.text.MessageFormat;

import org.w3c.dom.Element;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class BooleanParameter extends AbstractParameter<Boolean> {

	public static final String TYPE = "Boolean"; //$NON-NLS-1$
	private static final long serialVersionUID = 0L;

	private Boolean value = Boolean.FALSE;

	/**
	 * Empty constructor
	 */
	public BooleanParameter() {
		//
	}

	/**
	 * Default constructors
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public BooleanParameter(String id, String name, Boolean value) {
		super(id, name);
		setValue(value);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public BooleanParameter(Element element) {
		super.fromDom(element);

		String valueS = element.getAttribute(Tags.VALUE);
		if (StringHelper.isEmpty(valueS)) {
			String msg = MessageFormat.format("No value defined for {0}", this.id); //$NON-NLS-1$
			throw new StrolchException(msg);
		}

		setValue(parseFromString(valueS));
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@Override
	public Boolean getValue() {
		return this.value;
	}

	@Override
	public void setValue(Boolean value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public String getType() {
		return BooleanParameter.TYPE;
	}

	@Override
	public Parameter<Boolean> getClone() {
		BooleanParameter clone = new BooleanParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}

	public static Boolean parseFromString(String valueS) {
		return Boolean.valueOf(valueS);
	}
}
