package li.strolch.metrics;

import static li.strolch.model.StrolchModelConstants.STATE_VALUES;

import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;

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

	public void incr(Resource metric, int value, ZonedDateTime time, String interpretation, String uom) {
		incr(metric, value, time, interpretation, uom, ChronoUnit.HOURS);
	}

	public void incr(Resource metric, int value, ZonedDateTime time, String interpretation, String uom,
			ChronoUnit granularity) {
		long timeStamp = getTimeStamp(time, granularity);
		ITimeVariable<IntegerValue> timeEvolution = getIntegerMetric(metric, interpretation, uom).getTimeEvolution();
		ITimeValue<IntegerValue> currentValue = timeEvolution.getValueAt(timeStamp);
		if (currentValue != null && currentValue.getTime() == timeStamp) {
			timeEvolution.setValueAt(timeStamp, currentValue.getValue().add(value));
		} else {
			timeEvolution.setValueAt(timeStamp, getValue(value));
		}
		tx().update(metric);
	}

	public void add(Resource metric, int value, ZonedDateTime time, String interpretation, String uom) {
		add(metric, value, time, interpretation, uom, ChronoUnit.HOURS);
	}

	public void add(Resource metric, int value, ZonedDateTime time, String interpretation, String uom,
			ChronoUnit granularity) {
		long timeStamp = getTimeStamp(time, granularity);
		ITimeVariable<IntegerListValue> timeEvolution = getIntegerListMetric(metric, interpretation,
				uom).getTimeEvolution();
		ITimeValue<IntegerListValue> currentValue = timeEvolution.getValueAt(timeStamp);
		if (currentValue != null && currentValue.getTime() == timeStamp) {
			timeEvolution.setValueAt(timeStamp, currentValue.getValue().add(List.of(value)));
		} else {
			timeEvolution.setValueAt(timeStamp, getValueList(value));
		}
		tx().update(metric);
	}

	public void set(Resource metric, boolean value, String interpretation, String uom, ZonedDateTime time) {
		set(metric, value, time, interpretation, uom, ChronoUnit.HOURS);
	}

	public void set(Resource metric, boolean value, ZonedDateTime time, String interpretation, String uom,
			ChronoUnit granularity) {
		long timeStamp = getTimeStamp(time, granularity);
		ITimeVariable<BooleanValue> timeEvolution = getBooleanMetric(metric, interpretation, uom).getTimeEvolution();
		timeEvolution.setValueAt(timeStamp, getValue(value));
		tx().update(metric);
	}

	protected IntegerTimedState getIntegerMetric(Resource metric, String interpretation, String uom) {
		IntegerTimedState metricT = metric.getTimedState(STATE_VALUES, false);
		if (metricT == null) {
			metricT = new IntegerTimedState(STATE_VALUES, "Values");
			metricT.setInterpretation(interpretation);
			metricT.setUom(uom);
			metric.addTimedState(metricT);
		}

		return metricT;
	}

	protected BooleanTimedState getBooleanMetric(Resource metric, String interpretation, String uom) {
		BooleanTimedState metricT = metric.getTimedState(STATE_VALUES, false);
		if (metricT == null) {
			metricT = new BooleanTimedState(STATE_VALUES, "Values");
			metricT.setInterpretation(interpretation);
			metricT.setUom(uom);
			metric.addTimedState(metricT);
		}

		return metricT;
	}

	protected IntegerListTimedState getIntegerListMetric(Resource metric, String interpretation, String uom) {
		IntegerListTimedState metricT = metric.getTimedState(STATE_VALUES, false);
		if (metricT == null) {
			metricT = new IntegerListTimedState(STATE_VALUES, "Values");
			metricT.setInterpretation(interpretation);
			metricT.setUom(uom);
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

	protected long getTimeStamp(ZonedDateTime time, ChronoUnit granularity) {
		return time.truncatedTo(granularity).toInstant().toEpochMilli();
	}

	protected int getDurationSeconds(Duration duration) {
		return (int) duration.abs().toSeconds();
	}
}
