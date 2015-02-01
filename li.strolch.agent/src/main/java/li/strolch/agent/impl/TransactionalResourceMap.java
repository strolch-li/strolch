package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_RESOURCE_REF;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import java.text.MessageFormat;
import java.util.List;

import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class TransactionalResourceMap extends TransactionalElementMap<Resource> implements ResourceMap {

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {

		if (!refP.getInterpretation().equals(INTERPRETATION_RESOURCE_REF)) {
			String msg = "{0} is not an Resource reference as its interpretation is not {1}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getLocator(), INTERPRETATION_RESOURCE_REF));
		}

		if (refP.getUom().equals(UOM_NONE)) {
			String msg = "{0} is not an Resource reference as its UOM is not set to a type!"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getLocator()));
		}
	}

	@Override
	protected ResourceDao getDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getResourceDao(tx);
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ResourceQuery query, ResourceVisitor<U> resourceVisitor) {
		return getDao(tx).doQuery(query, resourceVisitor);
	}
}
