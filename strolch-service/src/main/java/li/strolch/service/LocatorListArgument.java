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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import li.strolch.model.Locator;
import li.strolch.service.api.ServiceArgument;

import java.util.List;

public class LocatorListArgument extends ServiceArgument {
	public List<Locator> locators;

	@Override
	public JsonElement toJson() {
		if (this.locators.isEmpty())
			return new JsonArray();
		JsonArray jsonArray = new JsonArray();
		this.locators.forEach(l -> jsonArray.add(l.toString()));
		return jsonArray;
	}
}
