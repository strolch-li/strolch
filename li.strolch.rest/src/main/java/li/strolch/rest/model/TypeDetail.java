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

	@XmlElements({ @XmlElement(name = "orders", type = OrderOverview.class),
			@XmlElement(name = "resources", type = ResourceOverview.class) })
	private List<StrolchElementOverview> elementOverviews;

	public TypeDetail() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param type
	 * @param elementOverviews
	 */
	public TypeDetail(String type, List<StrolchElementOverview> elementOverviews) {
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.elementOverviews == null) ? 0 : this.elementOverviews.hashCode());
		result = prime * result + ((this.type == null) ? 0 : this.type.hashCode());
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
		TypeDetail other = (TypeDetail) obj;
		if (this.elementOverviews == null) {
			if (other.elementOverviews != null)
				return false;
		} else if (!this.elementOverviews.equals(other.elementOverviews))
			return false;
		if (this.type == null) {
			if (other.type != null)
				return false;
		} else if (!this.type.equals(other.type))
			return false;
		return true;
	}
}
