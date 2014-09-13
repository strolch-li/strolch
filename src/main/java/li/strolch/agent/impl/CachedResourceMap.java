package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_RESOURCE_REF;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import java.text.MessageFormat;

import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class CachedResourceMap extends CachedElementMap<Resource> implements ResourceMap {

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		if (!refP.getInterpretation().equals(INTERPRETATION_RESOURCE_REF)) {
			String msg = MessageFormat.format("{0} is not an Resource reference as its interpretation is not {1}", //$NON-NLS-1$
					refP.getLocator(), INTERPRETATION_RESOURCE_REF);
			throw new StrolchException(msg);
		}

		if (refP.getUom().equals(UOM_NONE)) {
			String msg = MessageFormat.format("{0} is not an Resource reference as its UOM is not set to a type!", //$NON-NLS-1$
					refP.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	protected ResourceDao getDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getResourceDao(tx);
	}
}
