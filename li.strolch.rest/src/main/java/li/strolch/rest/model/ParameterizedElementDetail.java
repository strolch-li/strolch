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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.model.ParameterizedElement;
import li.strolch.model.parameter.Parameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "ParameterBag")
public class ParameterizedElementDetail extends StrolchElementDetail {

	@XmlElement(name = "parameters", type = ParameterDetail.class)
	private List<ParameterDetail> parameters;

	public ParameterizedElementDetail() {
		// no-arg constructor for JAXB
	}

	public ParameterizedElementDetail(String id, String name, String type, List<ParameterDetail> parameters) {
		super(id, name, type);
		this.parameters = parameters;
	}

	public ParameterizedElementDetail(ParameterizedElement parameterizedElement) {
		super(parameterizedElement);

		List<Parameter<?>> parameters = parameterizedElement.getParameters();
		this.parameters = new ArrayList<>(parameters.size());
		for (Parameter<?> parameter : parameters) {
			this.parameters.add(new ParameterDetail(parameter));
		}
	}

	/**
	 * @return the parameters
	 */
	public List<ParameterDetail> getParameters() {
		return this.parameters;
	}

	/**
	 * @param parameters
	 *            the parameters to set
	 */
	public void setParameters(List<ParameterDetail> parameters) {
		this.parameters = parameters;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((this.parameters == null) ? 0 : this.parameters.hashCode());
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
		ParameterizedElementDetail other = (ParameterizedElementDetail) obj;
		if (this.parameters == null) {
			if (other.parameters != null)
				return false;
		} else if (!this.parameters.equals(other.parameters))
			return false;
		return true;
	}
}
