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

import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "ElementMaps")
public class ElementMapsOverview {

	@XmlAttribute(name = "name")
	private String name;

	@XmlAttribute(name = "elementMapType")
	private ElementMapType elementMapType;

	@XmlAttribute(name = "nrOfElements")
	private long nrOfElements;

	@XmlElement(name = "types")
	private Set<String> types;

	public ElementMapsOverview() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param elementMapType
	 */
	public ElementMapsOverview(ElementMapType elementMapType) {
		this.elementMapType = elementMapType;
		this.name = elementMapType.getName();
	}

	/**
	 * 
	 * @param elementMapType
	 * @param nrOfElements
	 * @param types
	 */
	public ElementMapsOverview(ElementMapType elementMapType, long nrOfElements, Set<String> types) {
		this(elementMapType);
		this.nrOfElements = nrOfElements;
		this.types = types;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the elementMapType
	 */
	public ElementMapType getElementMapType() {
		return this.elementMapType;
	}

	/**
	 * @param elementMapType
	 *            the elementMapType to set
	 */
	public void setElementMapType(ElementMapType elementMapType) {
		this.elementMapType = elementMapType;
	}

	/**
	 * @return the types
	 */
	public Set<String> getTypes() {
		return this.types;
	}

	/**
	 * @param types
	 *            the types to set
	 */
	public void setTypes(Set<String> types) {
		this.types = types;
	}

	/**
	 * @return the nrOfElements
	 */
	public long getNrOfElements() {
		return this.nrOfElements;
	}

	/**
	 * @param nrOfElements
	 *            the nrOfElements to set
	 */
	public void setNrOfElements(long nrOfElements) {
		this.nrOfElements = nrOfElements;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.elementMapType == null) ? 0 : this.elementMapType.hashCode());
		result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
		result = prime * result + (int) (this.nrOfElements ^ (this.nrOfElements >>> 32));
		result = prime * result + ((this.types == null) ? 0 : this.types.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ElementMapsOverview other = (ElementMapsOverview) obj;
		if (this.elementMapType != other.elementMapType)
			return false;
		if (this.name == null) {
			if (other.name != null)
				return false;
		} else if (!this.name.equals(other.name))
			return false;
		if (this.nrOfElements != other.nrOfElements)
			return false;
		if (this.types == null) {
			if (other.types != null)
				return false;
		} else if (!this.types.equals(other.types))
			return false;
		return true;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("ElementMapsOverview [name=");
		sb.append(this.name);
		sb.append(", elementMapType=");
		sb.append(this.elementMapType);
		sb.append(", nrOfElements=");
		sb.append(this.nrOfElements);
		sb.append(", types=");
		sb.append(this.types);
		sb.append("]");
		return sb.toString();
	}
}
