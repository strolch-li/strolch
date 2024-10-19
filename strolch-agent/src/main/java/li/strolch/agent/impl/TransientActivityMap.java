package li.strolch.agent.impl;

import li.strolch.agent.api.ActivityMap;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ACTIVITY_REF;

public class TransientActivityMap extends TransientElementMap<Activity> implements ActivityMap {

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ACTIVITY_REF, refP);
	}
}
