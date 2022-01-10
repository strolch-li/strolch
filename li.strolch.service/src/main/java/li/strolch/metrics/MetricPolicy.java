package li.strolch.metrics;

import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.model.builder.BuilderHelper.buildParamName;

import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;

import li.strolch.model.Resource;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.IntegerListTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.IntegerListValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class MetricPolicy extends StrolchPolicy {

	public MetricPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public void incr(Resource metric, int value, ZonedDateTime time) {
		long timeStamp = getTimeStamp(time);
		getIntegerMetric(metric).applyChange(getValueChange(timeStamp, value), true);
		tx().update(metric);
	}

	public void add(Resource metric, int value, ZonedDateTime time) {
		long timeStamp = getTimeStamp(time);
		getIntegerListMetric(metric).applyChange(getValueChangeList(timeStamp, value), true);
		tx().update(metric);
	}

	public void set(Resource metric, boolean value, ZonedDateTime time) {
		long timeStamp = getTimeStamp(time);
		getBooleanMetric(metric).applyChange(getValueChange(timeStamp, value), true);
		tx().update(metric);
	}

	protected IntegerTimedState getIntegerMetric(Resource metric) {
		IntegerTimedState metricT = metric.getTimedState(STATE_VALUES, false);
		if (metricT == null) {
			metricT = new IntegerTimedState(STATE_VALUES, "Values");
			metric.addTimedState(metricT);
		}

		return metricT;
	}

	protected BooleanTimedState getBooleanMetric(Resource metric) {
		BooleanTimedState metricT = metric.getTimedState(STATE_VALUES, false);
		if (metricT == null) {
			metricT = new BooleanTimedState(STATE_VALUES, "Values");
			metric.addTimedState(metricT);
		}

		return metricT;
	}

	protected IntegerListTimedState getIntegerListMetric(Resource metric) {
		IntegerListTimedState metricT = metric.getTimedState(STATE_VALUES, false);
		if (metricT == null) {
			metricT = new IntegerListTimedState(STATE_VALUES, "Values");
			metric.addTimedState(metricT);
		}

		return metricT;
	}

	protected ValueChange<BooleanValue> getValueChange(long timeStamp, boolean value) {
		return new ValueChange<>(timeStamp, new BooleanValue(value));
	}

	protected ValueChange<IntegerValue> getValueChange(long timeStamp, int value) {
		return new ValueChange<>(timeStamp, new IntegerValue(value));
	}

	protected ValueChange<IntegerListValue> getValueChangeList(long timeStamp, int value) {
		return new ValueChange<>(timeStamp, new IntegerListValue(value));
	}

	protected long getTimeStamp(ZonedDateTime time) {
		time = time.truncatedTo(ChronoUnit.HOURS);
		return time.toInstant().toEpochMilli();
	}

	protected int getDurationSeconds(Duration duration) {
		return (int) duration.toSeconds();
	}
}
