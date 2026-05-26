package li.strolch.model.visitor;

import li.strolch.model.timevalue.impl.*;

public interface IValueVisitor<U> {

	U accept(FloatListValue floatListValue);

	U accept(LongValue longValue);

	U accept(IntegerListValue integerListValue);

	U accept(IntegerValue integerValue);

	U accept(BooleanValue booleanValue);

	U accept(StringSetValue stringSetValue);

	U accept(FloatValue floatValue);
}
