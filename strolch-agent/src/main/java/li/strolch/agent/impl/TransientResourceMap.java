package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_RESOURCE_REF;

import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;

public class TransientResourceMap extends TransientElementMap<Resource> implements ResourceMap {

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_RESOURCE_REF, refP);
	}
}
