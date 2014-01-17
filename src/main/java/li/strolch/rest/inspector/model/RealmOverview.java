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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "RealmOverview")
public class RealmOverview {

	@XmlAttribute(name = "realmName")
	private String realmName;
	@XmlAttribute(name = "size")
	private long size;

	public RealmOverview() {
		// no-arg constructor for JAXB
	}

	public RealmOverview(String realmName, long size) {
		this.realmName = realmName;
		this.size = size;

	}

	/**
	 * @return the realmName
	 */
	public String getRealmName() {
		return this.realmName;
	}

	/**
	 * @param realmName
	 *            the realmName to set
	 */
	public void setRealmName(String realmName) {
		this.realmName = realmName;
	}

	/**
	 * @return the size
	 */
	public long getSize() {
		return this.size;
	}

	/**
	 * @param size
	 *            the size to set
	 */
	public void setSize(long size) {
		this.size = size;
	}

}
