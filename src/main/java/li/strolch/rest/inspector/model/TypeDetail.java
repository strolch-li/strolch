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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "Types")
public class TypeDetail {

	@XmlAttribute(name = "type")
	private String type;

	@XmlElements(value = { @XmlElement(name = "Orders", type = OrderOverview.class),
			@XmlElement(name = "Resources", type = ResourceOverview.class) })
	private List<StrolchElementOverview> elementOverviews;

	public TypeDetail() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param type
	 * @param elementOverviews
	 */
	public TypeDetail(String type, List<StrolchElementOverview> elementOverviews) {
		super();
		this.type = type;
		this.elementOverviews = elementOverviews;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return this.type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * @return the elementOverviews
	 */
	public List<StrolchElementOverview> getElementOverviews() {
		return this.elementOverviews;
	}

	/**
	 * @param elementOverviews
	 *            the elementOverviews to set
	 */
	public void setElementOverviews(List<StrolchElementOverview> elementOverviews) {
		this.elementOverviews = elementOverviews;
	}
}
