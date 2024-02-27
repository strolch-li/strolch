package li.strolch.model.builder;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterBagContainer;
import li.strolch.model.builder.params.*;

import static li.strolch.model.builder.BuilderHelper.buildParamName;

public class BagBuilder<T extends ParameterBagContainerBuilder<T>> {

	private final T builder;
	private final String id;
	private final String name;
	private final String type;
	private final List<ParameterBuilder<?, ?, ?>> parameters;

	public BagBuilder(T builder, String id, String type) {
		this(builder, id, buildParamName(id), type);
	}

	public BagBuilder(T builder, String id, String name, String type) {
		this.builder = builder;
		this.id = id;
		this.name = name;
		this.type = type;
		this.parameters = new ArrayList<>();
	}

	public StringParamBuilder<T> string(String id) {
		return string(id, buildParamName(id));
	}

	public StringParamBuilder<T> string(String id, String name) {
		StringParamBuilder<T> builder = new StringParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public TextParamBuilder<T> text(String id) {
		return text(id, buildParamName(id));
	}

	public TextParamBuilder<T> text(String id, String name) {
		TextParamBuilder<T> builder = new TextParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public BooleanParamBuilder<T> booleanB(String id) {
		return booleanB(id, buildParamName(id));
	}

	public BooleanParamBuilder<T> booleanB(String id, String name) {
		BooleanParamBuilder<T> builder = new BooleanParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public IntegerParamBuilder<T> integer(String id) {
		return integer(id, buildParamName(id));
	}

	public IntegerParamBuilder<T> integer(String id, String name) {
		IntegerParamBuilder<T> builder = new IntegerParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public LongParamBuilder<T> longB(String id) {
		return longB(id, buildParamName(id));
	}

	public LongParamBuilder<T> longB(String id, String name) {
		LongParamBuilder<T> builder = new LongParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public FloatParamBuilder<T> floatB(String id) {
		return floatB(id, buildParamName(id));
	}

	public FloatParamBuilder<T> floatB(String id, String name) {
		FloatParamBuilder<T> builder = new FloatParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public DateParamBuilder<T> date(String id) {
		return date(id, buildParamName(id));
	}

	public DateParamBuilder<T> date(String id, String name) {
		DateParamBuilder<T> builder = new DateParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public DurationParamBuilder<T> duration(String id) {
		return duration(id, buildParamName(id));
	}

	public DurationParamBuilder<T> duration(String id, String name) {
		DurationParamBuilder<T> builder = new DurationParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public StringListParamBuilder<T> stringList(String id) {
		return stringList(id, buildParamName(id));
	}

	public StringListParamBuilder<T> stringList(String id, String name) {
		StringListParamBuilder<T> builder = new StringListParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public IntegerListParamBuilder<T> integerList(String id) {
		return integerList(id, buildParamName(id));
	}

	public IntegerListParamBuilder<T> integerList(String id, String name) {
		IntegerListParamBuilder<T> builder = new IntegerListParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public LongListParamBuilder<T> longList(String id) {
		return longList(id, buildParamName(id));
	}

	public LongListParamBuilder<T> longList(String id, String name) {
		LongListParamBuilder<T> builder = new LongListParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public FloatListParamBuilder<T> floatList(String id) {
		return floatList(id, buildParamName(id));
	}

	public FloatListParamBuilder<T> floatList(String id, String name) {
		FloatListParamBuilder<T> builder = new FloatListParamBuilder<>(this, id, name);
		this.parameters.add(builder);
		return builder;
	}

	public T endBag() {
		return this.builder;
	}

	public void applyBag(ParameterBagContainer element) {
		ParameterBag bag = new ParameterBag(this.id, this.name, this.type);
		this.parameters.forEach(b -> bag.addParameter(b.build()));
		element.addParameterBag(bag);
	}
}
