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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.service.JsonServiceResult;

import static li.strolch.model.Tags.Json.DATA;

@Schema(description = "Represents the result of a service execution which returns JSON data")
public class JsonServiceResultResponse extends ServiceResultResponse {

	@Schema(description = "Data returned by the service for the consumer", nullable = true, type = "object")
	private final JsonElement data;

	public JsonServiceResultResponse(JsonServiceResult serviceResult, boolean withStackTrace) {
		super(serviceResult, withStackTrace);
		this.data = serviceResult.getResult();
	}

	public JsonElement getData() {
		return this.data;
	}

	@Override
	public JsonObject toJsonObject() {
		JsonObject json = super.toJsonObject();
		json.add(DATA, this.data);
		return json;
	}
}
