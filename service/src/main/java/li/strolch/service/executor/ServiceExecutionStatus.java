/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.service.executor;

import com.google.gson.JsonObject;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServiceExecutionStatus {

	private final String serviceName;
	private volatile boolean started;
	private volatile ServiceResult result;

	public ServiceExecutionStatus(String serviceName) {
		this.serviceName = serviceName;
	}

	public String getServiceName() {
		return serviceName;
	}

	public synchronized ServiceResult getResult() {
		return result;
	}

	public synchronized void setResult(ServiceResult svcResult) {
		this.result = svcResult;
	}

	public String getMsg() {
		if (this.result == null)
			return StringHelper.DASH;
		if (this.result.getMessage() == null)
			return StringHelper.DASH;
		return this.result.getMessage();
	}

	public String getState() {
		if (this.result == null)
			return StringHelper.DASH;
		return this.result.getState().name();
	}

	public boolean isDone() {
		return this.result != null;
	}

	public synchronized boolean isStarted() {
		return started;
	}

	public synchronized void started() {
		this.started = true;
	}

	public JsonObject toJson() {
		JsonObject json = new JsonObject();

		json.addProperty("serviceName", serviceName);
		json.addProperty("started", started);
		json.add("result", result.toJson());

		return json;
	}
}