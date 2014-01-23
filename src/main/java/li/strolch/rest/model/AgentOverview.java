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
@XmlRootElement(name = "Agent")
public class AgentOverview {

	@XmlElement(name = "realms")
	private List<RealmOverview> realms;

	public AgentOverview() {
		// no-arg constructor for JAXB
	}

	public AgentOverview(List<RealmOverview> realms) {
		this.realms = realms;
	}

	/**
	 * @return the realms
	 */
	public List<RealmOverview> getRealms() {
		return this.realms;
	}

	/**
	 * @param realms
	 *            the realms to set
	 */
	public void setRealms(List<RealmOverview> realms) {
		this.realms = realms;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.realms == null) ? 0 : this.realms.hashCode());
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
		AgentOverview other = (AgentOverview) obj;
		if (this.realms == null) {
			if (other.realms != null)
				return false;
		} else if (!this.realms.equals(other.realms))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "AgentOverview [realms=" + this.realms + "]";
	}
}
