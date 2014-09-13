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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "Realm")
public class RealmDetail {

	@XmlElement(name = "elementMaps")
	private List<ElementMapsOverview> elementMapOverviews;

	public RealmDetail() {
		// no-arg constructor for JAXB
	}

	public RealmDetail(List<ElementMapsOverview> elementMapOverviews) {
		this.elementMapOverviews = elementMapOverviews;
	}

	/**
	 * @return the elementMapOverviews
	 */
	public List<ElementMapsOverview> getElementMapOverviews() {
		return this.elementMapOverviews;
	}

	/**
	 * @param elementMapOverviews
	 *            the elementMapOverviews to set
	 */
	public void setElementMapOverviews(List<ElementMapsOverview> elementMapOverviews) {
		this.elementMapOverviews = elementMapOverviews;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.elementMapOverviews == null) ? 0 : this.elementMapOverviews.hashCode());
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
		RealmDetail other = (RealmDetail) obj;
		if (this.elementMapOverviews == null) {
			if (other.elementMapOverviews != null)
				return false;
		} else if (!this.elementMapOverviews.equals(other.elementMapOverviews))
			return false;
		return true;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("RealmDetail [elementMapOverviews=");
		sb.append(this.elementMapOverviews);
		sb.append("]");
		return sb.toString();
	}
}
