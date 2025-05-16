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

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.utils.I18nMessage;

import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.StringHelper.*;

@Schema(description = "Represents a standard Strolch response")
public class StrolchResponse {

	@Schema(description = "Message describing the result.", example = "Operation completed successfully.")
	private final String msg;

	@Schema(description = "Internationalization message details if available.", nullable = true)
	private final I18nMessageResponse i18n;

	@Schema(description = "Message describing the result.", nullable = true, type = "object")
	private final Object data;

	private final JsonElement _data;

	public StrolchResponse(String msg, I18nMessageResponse i18n, JsonElement data) {
		this.msg = msg;
		this.i18n = i18n;
		this.data = data;
		this._data = data;
	}

	public String getMsg() {
		return this.msg;
	}

	public I18nMessageResponse getI18n() {
		return this.i18n;
	}

	public Object getData() {
		return this.data;
	}

	public static StrolchResponse valueOf() {
		return new StrolchResponse(DASH, null, null);
	}

	public static StrolchResponse valueOf(String msg) {
		return new StrolchResponse(msg, null, null);
	}

	public static StrolchResponse valueOf(String msg, I18nMessage i18nMessage) {
		return new StrolchResponse(msg, new I18nMessageResponse(i18nMessage), null);
	}

	public static StrolchResponse valueOf(String msg, JsonElement data) {
		return new StrolchResponse(msg, null, data);
	}

	public static StrolchResponse valueOf(String msg, I18nMessage i18nMessage, JsonElement data) {
		return new StrolchResponse(msg, new I18nMessageResponse(i18nMessage), data);
	}

	public static StrolchResponse valueOf(JsonElement data) {
		return new StrolchResponse(DASH, null, data);
	}

	public String toJson() {
		return new Gson().toJson(toJsonObject());
	}

	public JsonObject toJsonObject() {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, isEmpty(trimOrEmpty(this.msg)) ? DASH : this.msg);
		if (this.i18n != null)
			response.add(I18N, this.i18n.toJson());
		if (this._data != null)
			response.add(DATA, this._data);
		return response;
	}
}
