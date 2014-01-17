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
		super();
		this.elementMapType = elementMapType;
		this.name = elementMapType.getName();
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
}
