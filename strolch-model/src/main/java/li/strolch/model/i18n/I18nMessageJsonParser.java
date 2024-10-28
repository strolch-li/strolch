/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import li.strolch.utils.I18nMessage;

import java.util.Properties;
import java.util.Set;

public class I18nMessageJsonParser {

	public I18nMessage parse(JsonObject messageJ) {

		String key = messageJ.get(Tags.Json.KEY).getAsString();
		String bundle = messageJ.get(Tags.Json.BUNDLE).getAsString();
		String message = messageJ.get(Tags.Json.MESSAGE).getAsString();

		Properties properties = new Properties();
		if (messageJ.has(Tags.Json.VALUES)) {
			JsonArray valuesJ = messageJ.getAsJsonArray(Tags.Json.VALUES);
			for (JsonElement jsonElement : valuesJ) {
				JsonObject valueJ = jsonElement.getAsJsonObject();

				Set<String> keys = valueJ.keySet();
				for (String propertyName : keys) {
					properties.setProperty(propertyName, valueJ.get(propertyName).getAsString());
				}
			}
		}

		return new I18nMessage(bundle, key, properties, message);
	}
}
