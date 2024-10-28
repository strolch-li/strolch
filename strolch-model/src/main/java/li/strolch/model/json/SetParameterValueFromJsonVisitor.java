package li.strolch.model.json;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.parameter.*;
import li.strolch.model.visitor.ParameterVisitor;
import li.strolch.utils.iso8601.ISO8601;
import li.strolch.utils.time.PeriodDuration;

public class SetParameterValueFromJsonVisitor implements ParameterVisitor<Void> {

	private final JsonObject container;
	private final boolean ignoreMissing;

	public SetParameterValueFromJsonVisitor(JsonObject container, boolean ignoreMissing) {
		this.container = container;
		this.ignoreMissing = ignoreMissing;
	}

	private boolean isMissing(Parameter<?> param) {
		if (this.ignoreMissing && !this.container.has(param.getId()))
			return true;
		if (!this.container.has(param.getId()))
			throw new IllegalStateException("Field " + param.getId() + " is missing on json data!");
		return false;
	}

	private boolean isNotSet(Parameter<?> param) {
		return !this.container.has(param.getId()) || this.container.get(param.getId()).isJsonNull();
	}

	@Override
	public Void visitBooleanParam(BooleanParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(this.container.get(param.getId()).getAsBoolean());
		return null;
	}

	@Override
	public Void visitDateParam(DateParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(ISO8601.parseToDate(this.container.get(param.getId()).getAsString()));
		return null;
	}

	@Override
	public Void visitDurationParam(DurationParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(PeriodDuration.parse(this.container.get(param.getId()).getAsString()));
		return null;
	}

	@Override
	public Void visitFloatParam(FloatParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(this.container.get(param.getId()).getAsDouble());
		return null;
	}

	@Override
	public Void visitIntegerParam(IntegerParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(this.container.get(param.getId()).getAsInt());
		return null;
	}

	@Override
	public Void visitLongParam(LongParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(this.container.get(param.getId()).getAsLong());
		return null;
	}

	@Override
	public Void visitStringParam(StringParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(this.container.get(param.getId()).getAsString());
		return null;
	}

	@Override
	public Void visitTextParam(TextParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param))
			param.clear();
		else
			param.setValue(this.container.get(param.getId()).getAsString());
		return null;
	}

	@Override
	public Void visitStringListParam(StringListParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param)) {
			param.clear();
		} else {
			JsonElement listJ = this.container.get(param.getId());
			param.clear();
			if (listJ.isJsonPrimitive())
				param.setValueFromString(listJ.getAsString());
			else
				listJ.getAsJsonArray().forEach(e -> param.addValue(e.getAsString()));
		}
		return null;
	}

	@Override
	public Void visitIntegerListParam(IntegerListParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param)) {
			param.clear();
		} else {
			JsonElement listJ = this.container.get(param.getId());
			param.clear();
			if (listJ.isJsonPrimitive())
				param.setValueFromString(listJ.getAsString());
			else
				listJ.getAsJsonArray().forEach(e -> param.addValue(e.getAsInt()));
		}
		return null;
	}

	@Override
	public Void visitFloatListParam(FloatListParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param)) {
			param.clear();
		} else {
			JsonElement listJ = this.container.get(param.getId());
			param.clear();
			if (listJ.isJsonPrimitive())
				param.setValueFromString(listJ.getAsString());
			else
				listJ.getAsJsonArray().forEach(e -> param.addValue(e.getAsDouble()));
		}
		return null;
	}

	@Override
	public Void visitLongListParam(LongListParameter param) {
		if (isMissing(param))
			return null;
		if (isNotSet(param)) {
			param.clear();
		} else {
			JsonElement listJ = this.container.get(param.getId());
			param.clear();
			if (listJ.isJsonPrimitive())
				param.setValueFromString(listJ.getAsString());
			else
				listJ.getAsJsonArray().forEach(e -> param.addValue(e.getAsLong()));
		}
		return null;
	}
}
