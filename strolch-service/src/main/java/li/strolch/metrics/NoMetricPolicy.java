package li.strolch.metrics;

import li.strolch.persistence.api.StrolchTransaction;

public class NoMetricPolicy extends MetricPolicy {

	public NoMetricPolicy(StrolchTransaction tx) {
		super(tx);
	}
}
