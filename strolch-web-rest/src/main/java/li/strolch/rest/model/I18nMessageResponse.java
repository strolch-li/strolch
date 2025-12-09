/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.model.Tags;
import li.strolch.utils.I18nMessage;

import java.util.HashMap;
import java.util.Map;

@Schema(description = "Represents an i18n message")
public class I18nMessageResponse {

	@Schema(description = "The key of the i18n message")
	private final String key;
	@Schema(description = "The formatted i18n message")
	private final String message;
	@Schema(description = "The stack trace if available")
	private final String exception;

	@Schema(description = "formatting values", type = "object")
	private final Map<String, String> values;

	protected I18nMessageResponse(I18nMessage i18nMessage) {
		this.key = i18nMessage.getKey();
		this.message = i18nMessage.getMessage();
		this.exception = i18nMessage.getStackTrace();
		this.values = new HashMap<>(i18nMessage.getValues());
	}

	public String getKey() {
		return key;
	}

	public String getMessage() {
		return message;
	}

	public String getException() {
		return exception;
	}

	public Map<String, String> getValues() {
		return values;
	}

	public JsonObject toJson() {
		JsonObject json = new JsonObject();

		json.addProperty(Tags.Json.KEY, this.key);
		json.addProperty(Tags.Json.MESSAGE, this.message);
		json.addProperty(Tags.Json.EXCEPTION, this.exception);

		if (!this.values.isEmpty()) {
			JsonObject valuesJ = new JsonObject();
			this.values.forEach(valuesJ::addProperty);
			json.add(Tags.Json.VALUES, valuesJ);
		}

		return json;
	}
}
