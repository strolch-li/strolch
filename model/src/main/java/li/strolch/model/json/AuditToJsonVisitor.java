package li.strolch.model.json;

import com.google.gson.JsonObject;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class AuditToJsonVisitor implements AuditVisitor<JsonObject> {

	@Override
	public JsonObject visitAudit(Audit audit) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("id", audit.getId());
		jsonObject.addProperty("username", audit.getUsername());
		jsonObject.addProperty("firstname", audit.getFirstname());
		jsonObject.addProperty("lastname", audit.getLastname());
		jsonObject.addProperty("date", ISO8601FormatFactory.getInstance().formatDate(audit.getDate()));
		jsonObject.addProperty("elementType", audit.getElementType());
		jsonObject.addProperty("elementSubType", audit.getElementSubType());
		jsonObject.addProperty("elementAccessed", audit.getElementAccessed());
		jsonObject.addProperty("newVersion", ISO8601FormatFactory.getInstance().formatDate(audit.getNewVersion()));
		jsonObject.addProperty("action", audit.getAction());
		jsonObject.addProperty("accessType", audit.getAccessType().name());

		return jsonObject;
	}
}
