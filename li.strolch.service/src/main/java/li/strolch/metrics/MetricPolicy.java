package li.strolch.metrics;

import static li.strolch.model.StrolchModelConstants.STATE_VALUES;

import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;

import li.strolch.model.Resource;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.IntegerListTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.IntegerListValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class MetricPolicy extends StrolchPolicy {

	public MetricPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public void incr(Resource metric, int value, ZonedDateTime time) {
		long timeStamp = getTimeStamp(time);
		ITimeVariable<IntegerValue> timeEvolution = getIntegerMetric(metric).getTimeEvolution();
		ITimeValue<IntegerValue> currentValue = timeEvolution.getValueAt(timeStamp);
		if (currentValue != null && currentValue.getTime() == timeStamp) {
			timeEvolution.setValueAt(timeStamp, currentValue.getCopy().add(getValue(value)).getValue());
		} else {
			timeEvolution.setValueAt(timeStamp, getValue(value));
		}
		timeEvolution.compact();
		tx().update(metric);
	}

	public void add(Resource metric, int value, ZonedDateTime time) {
		long timeStamp = getTimeStamp(time);
		ITimeVariable<IntegerListValue> timeEvolution = getIntegerListMetric(metric).getTimeEvolution();
		ITimeValue<IntegerListValue> currentValue = timeEvolution.getValueAt(timeStamp);
		if (currentValue != null && currentValue.getTime() == timeStamp) {
			timeEvolution.setValueAt(timeStamp, currentValue.getCopy().add(getValueList(value)).getValue());
		} else {
			timeEvolution.setValueAt(timeStamp, getValueList(value));
		}
		timeEvolution.compact();
		tx().update(metric);
	}

	public void set(Resource metric, boolean value, ZonedDateTime time) {
		long timeStamp = getTimeStamp(time);
		ITimeVariable<BooleanValue> timeEvolution = getBooleanMetric(metric).getTimeEvolution();
		ITimeValue<BooleanValue> currentValue = timeEvolution.getValueAt(timeStamp);
		if (currentValue != null && currentValue.getTime() == timeStamp) {
			timeEvolution.setValueAt(timeStamp, currentValue.getCopy().add(getValue(value)).getValue());
		} else {
			timeEvolution.setValueAt(timeStamp, getValue(value));
		}
		timeEvolution.compact();
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

	protected BooleanValue getValue(boolean value) {
		return new BooleanValue(value);
	}

	protected IntegerValue getValue(int value) {
		return new IntegerValue(value);
	}

	protected IntegerListValue getValueList(int value) {
		return new IntegerListValue(value);
	}

	protected long getTimeStamp(ZonedDateTime time) {
		time = time.truncatedTo(ChronoUnit.HOURS);
		return time.toInstant().toEpochMilli();
	}

	protected int getDurationSeconds(Duration duration) {
		return (int) duration.toSeconds();
	}
}
