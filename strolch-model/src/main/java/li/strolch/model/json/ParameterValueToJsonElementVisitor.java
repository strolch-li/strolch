package li.strolch.model.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import li.strolch.model.parameter.*;
import li.strolch.model.visitor.ParameterVisitor;

import java.util.stream.Stream;

public class ParameterValueToJsonElementVisitor implements ParameterVisitor<JsonElement> {

	@Override
	public JsonElement visitBooleanParam(BooleanParameter param) {
		return new JsonPrimitive(param.getValue());
	}

	@Override
	public JsonElement visitDateParam(DateParameter param) {
		return new JsonPrimitive(param.getValueAsString());
	}

	@Override
	public JsonElement visitDurationParam(DurationParameter param) {
		return new JsonPrimitive(param.getValueAsString());
	}

	@Override
	public JsonElement visitFloatParam(FloatParameter param) {
		return new JsonPrimitive(param.getValue());
	}

	@Override
	public JsonElement visitIntegerParam(IntegerParameter param) {
		return new JsonPrimitive(param.getValue());
	}

	@Override
	public JsonElement visitLongParam(LongParameter param) {
		return new JsonPrimitive(param.getValue());
	}

	@Override
	public JsonElement visitStringParam(StringParameter param) {
		return new JsonPrimitive(param.getValue());
	}

	@Override
	public JsonElement visitTextParam(TextParameter param) {
		return new JsonPrimitive(param.getValue());
	}

	@Override
	public JsonElement visitStringListParam(StringListParameter param) {
		return toJsonArray(param.getValue().stream().map(JsonPrimitive::new));
	}

	@Override
	public JsonElement visitIntegerListParam(IntegerListParameter param) {
		return toJsonArray(param.getValue().stream().map(JsonPrimitive::new));
	}

	@Override
	public JsonElement visitFloatListParam(FloatListParameter param) {
		return toJsonArray(param.getValue().stream().map(JsonPrimitive::new));
	}

	@Override
	public JsonElement visitLongListParam(LongListParameter param) {
		return toJsonArray(param.getValue().stream().map(JsonPrimitive::new));
	}

	private static JsonArray toJsonArray(Stream<JsonPrimitive> stream) {
		return stream.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
	}
}
