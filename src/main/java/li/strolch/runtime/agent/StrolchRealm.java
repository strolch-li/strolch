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
package li.strolch.runtime.agent;

import li.strolch.model.query.StrolchQuery;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchRealm {

	public static final String DEFAULT_REALM = StrolchQuery.DEFAULT_REALM;

	private String realm;
	private ResourceMap resourceMap;
	private OrderMap orderMap;

	public StrolchRealm(String realm, ResourceMap resourceMap, OrderMap orderMap) {
		this.realm = realm;
		this.resourceMap = resourceMap;
		this.orderMap = orderMap;
	}

	/**
	 * @return the realm
	 */
	public String getRealm() {
		return this.realm;
	}

	/**
	 * @return the resourceMap
	 */
	public ResourceMap getResourceMap() {
		return this.resourceMap;
	}

	/**
	 * @return the orderMap
	 */
	public OrderMap getOrderMap() {
		return this.orderMap;
	}
}
