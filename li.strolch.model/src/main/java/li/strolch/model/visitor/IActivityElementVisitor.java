package li.strolch.model.visitor;

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.*;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;

public interface IActivityElementVisitor<U> extends StrolchElementVisitor<U> {

	@Override
	public default U visitResource(Resource resource) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + resource.getClass());
	}

	@Override
	public default U visitOrder(Order order) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + order.getClass());
	}

	@Override
	public default U visitBooleanParam(BooleanParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitDateParam(DateParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitDurationParam(DurationParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitFloatParam(FloatParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitIntegerParam(IntegerParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitLongParam(LongParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitStringParam(StringParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitStringListParam(StringListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitIntegerListParam(IntegerListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitFloatListParam(FloatListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitLongListParam(LongListParameter param) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + param.getClass());
	}

	@Override
	public default U visitBooleanState(BooleanTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitFloatState(FloatTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitIntegerState(IntegerTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitStringState(StringSetTimedState state) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + state.getClass());
	}

	@Override
	public default U visitParameterBag(ParameterBag bag) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + bag.getClass());
	}
}
