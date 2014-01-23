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
package li.strolch.rest.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.model.parameter.Parameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "Parameter")
public class ParameterDetail extends StrolchElementDetail {

	@XmlAttribute(name = "hidden")
	private boolean hidden;
	@XmlAttribute(name = "interpretation")
	private String interpretation;
	@XmlAttribute(name = "uom")
	private String uom;
	@XmlAttribute(name = "value")
	private String value;

	public ParameterDetail() {
		// no-arg constructor for JAXB
	}

	/**
	 * 
	 * @param id
	 * @param name
	 * @param type
	 * @param hidden
	 * @param interpretation
	 * @param uom
	 * @param value
	 */
	public ParameterDetail(String id, String name, String type, boolean hidden, String interpretation, String uom,
			String value) {
		super(id, name, type);
		this.hidden = hidden;
		this.interpretation = interpretation;
		this.uom = uom;
		this.value = value;
	}

	/**
	 * @param parameter
	 */
	public ParameterDetail(Parameter<?> parameter) {
		super(parameter);
		this.hidden = parameter.isHidden();
		this.interpretation = parameter.getInterpretation();
		this.uom = parameter.getUom();
		this.value = parameter.getValueAsString();
	}

	/**
	 * @return the hidden
	 */
	public boolean isHidden() {
		return this.hidden;
	}

	/**
	 * @param hidden
	 *            the hidden to set
	 */
	public void setHidden(boolean hidden) {
		this.hidden = hidden;
	}

	/**
	 * @return the interpretation
	 */
	public String getInterpretation() {
		return this.interpretation;
	}

	/**
	 * @param interpretation
	 *            the interpretation to set
	 */
	public void setInterpretation(String interpretation) {
		this.interpretation = interpretation;
	}

	/**
	 * @return the uom
	 */
	public String getUom() {
		return this.uom;
	}

	/**
	 * @param uom
	 *            the uom to set
	 */
	public void setUom(String uom) {
		this.uom = uom;
	}

	/**
	 * @return the value
	 */
	public String getValue() {
		return this.value;
	}

	/**
	 * @param value
	 *            the value to set
	 */
	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (this.hidden ? 1231 : 1237);
		result = prime * result + ((this.interpretation == null) ? 0 : this.interpretation.hashCode());
		result = prime * result + ((this.uom == null) ? 0 : this.uom.hashCode());
		result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		ParameterDetail other = (ParameterDetail) obj;
		if (this.hidden != other.hidden)
			return false;
		if (this.interpretation == null) {
			if (other.interpretation != null)
				return false;
		} else if (!this.interpretation.equals(other.interpretation))
			return false;
		if (this.uom == null) {
			if (other.uom != null)
				return false;
		} else if (!this.uom.equals(other.uom))
			return false;
		if (this.value == null) {
			if (other.value != null)
				return false;
		} else if (!this.value.equals(other.value))
			return false;
		return true;
	}
}
