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
package li.strolch.rest.inspector.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.ParameterBag;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "GroupedParameterizedElement")
@XmlSeeAlso({ ResourceDetail.class, OrderDetail.class })
public class GroupedParameterizedElementDetail extends StrolchElementDetail {

	@XmlElement(name = "parameterBags", type = ParameterizedElementDetail.class)
	private List<ParameterizedElementDetail> parameterizedElements;

	public GroupedParameterizedElementDetail() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param id
	 * @param name
	 * @param type
	 * @param parameterizedElements
	 */
	public GroupedParameterizedElementDetail(String id, String name, String type,
			List<ParameterizedElementDetail> parameterizedElements) {
		super(id, name, type);
		this.parameterizedElements = parameterizedElements;
	}

	/**
	 * @param strolchElement
	 */
	public GroupedParameterizedElementDetail(GroupedParameterizedElement groupedParameterizedElement) {
		super(groupedParameterizedElement);

		Set<String> bagKeySet = groupedParameterizedElement.getParameterBagKeySet();
		this.parameterizedElements = new ArrayList<>(bagKeySet.size());
		for (String bagId : bagKeySet) {
			ParameterBag parameterBag = groupedParameterizedElement.getParameterBag(bagId);
			this.parameterizedElements.add(new ParameterizedElementDetail(parameterBag));
		}
	}

	/**
	 * @return the parameterizedElements
	 */
	public List<ParameterizedElementDetail> getParameterizedElements() {
		return this.parameterizedElements;
	}

	/**
	 * @param parameterizedElements
	 *            the parameterizedElements to set
	 */
	public void setParameterizedElements(List<ParameterizedElementDetail> parameterizedElements) {
		this.parameterizedElements = parameterizedElements;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((this.parameterizedElements == null) ? 0 : this.parameterizedElements.hashCode());
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
		GroupedParameterizedElementDetail other = (GroupedParameterizedElementDetail) obj;
		if (this.parameterizedElements == null) {
			if (other.parameterizedElements != null)
				return false;
		} else if (!this.parameterizedElements.equals(other.parameterizedElements))
			return false;
		return true;
	}
}
