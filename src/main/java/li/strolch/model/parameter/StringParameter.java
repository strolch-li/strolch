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

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;

import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class StringParameter extends AbstractParameter<String> {

	public static final String UNDEFINED_VALUE = "-"; //$NON-NLS-1$
	public static final String TYPE = "String"; //$NON-NLS-1$
	private static final long serialVersionUID = 0L;

	private String value = UNDEFINED_VALUE;

	/**
	 * Empty constructor
	 * 
	 */
	public StringParameter() {
		//
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public StringParameter(String id, String name, String value) {
		super(id, name);
		setValue(value);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public StringParameter(Element element) {
		super.fromDom(element);

		String valueS = element.getAttribute(Tags.VALUE);
		if (StringHelper.isEmpty(valueS)) {
			String msg = MessageFormat.format("No value defined for {0}", this.id); //$NON-NLS-1$
			throw new StrolchException(msg);
		}

		setValue(valueS);
	}

	@Override
	public String getType() {
		return StringParameter.TYPE;
	}

	@Override
	public String getValue() {
		return this.value;
	}

	@Override
	public String getValueAsString() {
		return this.value;
	}

	@Override
	public void setValue(String value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public StringParameter getClone() {
		StringParameter clone = new StringParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}
}
