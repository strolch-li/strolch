/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.Locator;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * A {@link ServiceResult} which defines the result of an {@link AbstractService} to be a {@link JsonElement}. This is
 * often used in conjunction with REST APIs.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LocatorServiceResult extends ServiceResult {
	private Locator locator;
	private String objectType;
	private String type;
	private String id;

	public LocatorServiceResult() {
		// do nothing
	}

	public LocatorServiceResult(ServiceResultState state) {
		super(state);
	}

	public LocatorServiceResult(ServiceResultState state, String message) {
		super(state, message);
	}

	public LocatorServiceResult(StrolchRootElement rootElement) {
		super(ServiceResultState.SUCCESS);
		this.locator = rootElement.getLocator();
		this.objectType = rootElement.getObjectType();
		this.type = rootElement.getType();
		this.id = rootElement.getId();
	}

	public LocatorServiceResult(Locator locator) {
		super(ServiceResultState.SUCCESS);
		this.locator = locator;
		this.objectType = locator.get(0);
		this.type = locator.get(1);
		this.id = locator.get(2);
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty(Tags.Json.LOCATOR, this.locator.toString());
		jsonObject.addProperty(Tags.Json.OBJECT_TYPE, this.objectType);
		jsonObject.addProperty(Tags.Json.TYPE, this.type);
		jsonObject.addProperty(Tags.Json.ID, this.id);
		return jsonObject;
	}
}
