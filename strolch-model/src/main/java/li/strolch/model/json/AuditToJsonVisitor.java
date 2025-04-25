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

package li.strolch.model.json;

import com.google.gson.JsonObject;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.utils.iso8601.ISO8601;

public class AuditToJsonVisitor implements AuditVisitor<JsonObject> {

	private boolean withAdditionalData;

	public AuditToJsonVisitor withAdditionalData() {
		this.withAdditionalData = true;
		return this;
	}

	@Override
	public JsonObject visitAudit(Audit audit) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("id", audit.getId());
		jsonObject.addProperty("username", audit.getUsername());
		jsonObject.addProperty("date", ISO8601.toString(audit.getDate()));
		jsonObject.addProperty("elementType", audit.getElementType());
		jsonObject.addProperty("elementSubType", audit.getElementSubType());
		jsonObject.addProperty("elementAccessed", audit.getElementAccessed());
		if (audit.getNewVersion() != null)
			jsonObject.addProperty("newVersion", ISO8601.toString(audit.getNewVersion()));
		jsonObject.addProperty("action", audit.getAction());
		jsonObject.addProperty("accessType", audit.getAccessType().name());
		if (this.withAdditionalData)
			jsonObject.add("additionalData", audit.getAdditionalData());

		return jsonObject;
	}
}
