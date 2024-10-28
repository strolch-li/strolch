package li.strolch.agent.impl;

import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_RESOURCE_REF;

public class TransientResourceMap extends TransientElementMap<Resource> implements ResourceMap {

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_RESOURCE_REF, refP);
	}
}
