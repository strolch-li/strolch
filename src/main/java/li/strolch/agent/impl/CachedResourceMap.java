package li.strolch.agent.impl;

import static li.strolch.runtime.StrolchConstants.INTERPRETATION_RESOURCE_REF;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class CachedResourceMap extends CachedElementMap<Resource> implements ResourceMap {

	@Override
	public Resource getBy(StrolchTransaction tx, StringParameter refP) throws StrolchException {

		if (!refP.getInterpretation().equals(INTERPRETATION_RESOURCE_REF)) {
			throw new StrolchException(refP.getLocator()
					+ " is not an Resource reference as its interpretation is not " + INTERPRETATION_RESOURCE_REF);
		}

		if (refP.getUom().equals(Parameter.UOM_NONE)) {
			throw new StrolchException(refP.getLocator()
					+ " is not an Resource reference as its UOM is not set to a type!");
		}

		String type = refP.getUom();
		String id = refP.getValue();

		return getBy(tx, type, id);
	}

	@Override
	protected ResourceDao getDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getResourceDao(tx);
	}
}
