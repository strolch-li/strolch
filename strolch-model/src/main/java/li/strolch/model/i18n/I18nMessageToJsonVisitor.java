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

package li.strolch.model.i18n;

import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.I18nMessageVisitor;

import java.util.Map;
import java.util.Properties;

public class I18nMessageToJsonVisitor implements I18nMessageVisitor<JsonObject> {

	@Override
	public JsonObject visit(I18nMessage message) {
		JsonObject json = new JsonObject();

		json.addProperty(Tags.Json.KEY, message.getKey());
		json.addProperty(Tags.Json.MESSAGE, message.getMessage());
		json.addProperty(Tags.Json.EXCEPTION, message.getStackTrace());

		Map<String, String> values = message.getValues();
		if (!values.isEmpty()) {
			JsonObject valuesJ = new JsonObject();
			values.forEach(valuesJ::addProperty);
			json.add(Tags.Json.VALUES, valuesJ);
		}

		return json;
	}
}
