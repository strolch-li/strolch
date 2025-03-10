package li.strolch.service.parameter;

import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import li.strolch.model.Locator;
import li.strolch.model.Tags;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.service.api.ServiceArgument;

public class AddParameterArg extends ServiceArgument {

	public Locator locator;
	public Parameter<?> parameter;

	@Override
	public JsonElement toJson() {
		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty(Tags.Json.LOCATOR, this.locator == null ? "null" : this.locator.toString());
		jsonObject.add(Tags.Json.PARAMETER,
				this.parameter == null ? JsonNull.INSTANCE : this.parameter.accept(new StrolchElementToJsonVisitor()));
		return jsonObject;
	}
}
