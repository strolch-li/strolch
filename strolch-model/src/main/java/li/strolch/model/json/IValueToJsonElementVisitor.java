package li.strolch.model.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import li.strolch.model.timevalue.impl.*;
import li.strolch.model.visitor.IValueVisitor;

import java.util.stream.Stream;

public class IValueToJsonElementVisitor implements IValueVisitor<JsonElement> {

	@Override
	public JsonElement accept(IntegerValue value) {
		return new JsonPrimitive(value.getValue());
	}

	@Override
	public JsonElement accept(BooleanValue value) {
		return new JsonPrimitive(value.getValue());
	}

	@Override
	public JsonElement accept(FloatValue value) {
		return new JsonPrimitive(value.getValue());
	}

	@Override
	public JsonElement accept(FloatListValue listValue) {
		return toJsonArray(listValue.getValue().stream().map(JsonPrimitive::new));
	}

	@Override
	public JsonElement accept(IntegerListValue listValue) {
		return toJsonArray(listValue.getValue().stream().map(JsonPrimitive::new));
	}

	@Override
	public JsonElement accept(StringSetValue setValue) {
		return toJsonArray(setValue.getValue().stream().map(aString -> new JsonPrimitive(aString.getString())));
	}

	@Override
	public JsonElement accept(LongValue value) {
		return new JsonPrimitive(value.getValue());
	}

	private static JsonArray toJsonArray(Stream<JsonPrimitive> stream) {
		return stream.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
	}
}
