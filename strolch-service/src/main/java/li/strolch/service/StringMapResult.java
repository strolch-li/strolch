/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.service;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

import java.util.HashMap;
import java.util.Map;

public class StringMapResult extends ServiceResult {

	private Map<String, String> map = new HashMap<>();

	public StringMapResult(ServiceResultState state) {
		super(state);
	}

	public StringMapResult(ServiceResultState state, String msg) {
		super(state, msg);
	}

	public StringMapResult(String key, String value) {
		super(ServiceResultState.SUCCESS);
		this.map = new HashMap<>();
		this.map.put(key, value);
	}

	public StringMapResult(String key1, String value1, String key2, String value2) {
		super(ServiceResultState.SUCCESS);
		this.map = new HashMap<>();
		this.map.put(key1, value1);
		this.map.put(key2, value2);
	}

	public Map<String, String> getMap() {
		return this.map;
	}

	public void put(String key, String value) {
		this.map.put(key, value);
	}
}
