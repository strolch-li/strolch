package li.strolch.service.parameter;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.Locator;
import li.strolch.model.Tags;
import li.strolch.service.api.ServiceArgument;

public class SetParameterArg extends ServiceArgument {
	public Locator locator;

	public String name;
	public String interpretation;
	public String uom;
	public Boolean hidden;
	public Integer index;

	public String valueAsString;

	@Override
	public JsonElement toJson() {
		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty(Tags.Json.LOCATOR, this.locator == null ? "null" : this.locator.toString());
		if (this.name != null)
			jsonObject.addProperty(Tags.Json.NAME, this.name);
		if (this.interpretation != null)
			jsonObject.addProperty(Tags.Json.INTERPRETATION, this.interpretation);
		if (this.uom != null)
			jsonObject.addProperty(Tags.Json.UOM, this.uom);
		if (this.hidden != null)
			jsonObject.addProperty(Tags.Json.HIDDEN, this.hidden);
		if (this.index != null)
			jsonObject.addProperty(Tags.Json.INDEX, this.index);
		if (this.valueAsString != null)
			jsonObject.addProperty(Tags.Json.VALUE, this.valueAsString);
		return jsonObject;
	}
}
