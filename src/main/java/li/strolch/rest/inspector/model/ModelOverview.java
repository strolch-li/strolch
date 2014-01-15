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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class ModelOverview {

	@XmlElement
	private List<ElementMapOverview> elementMapOverviews;

	public ModelOverview() {
		this.elementMapOverviews = new ArrayList<>();
	}

	/**
	 * @return the elementMapOverviews
	 */
	public List<ElementMapOverview> getElementMapOverviews() {
		return this.elementMapOverviews;
	}

	/**
	 * @param elementMapOverviews
	 *            the elementMapOverviews to set
	 */
	public void setElementMapOverviews(List<ElementMapOverview> elementMapOverviews) {
		this.elementMapOverviews = elementMapOverviews;
	}
}
