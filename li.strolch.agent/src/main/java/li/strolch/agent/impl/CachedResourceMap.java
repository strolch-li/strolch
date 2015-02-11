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
import li.strolch.persistence.inmemory.InMemoryResourceDao;

public class CachedResourceMap extends CachedElementMap<Resource> implements ResourceMap {

	private ResourceDao cachedDao;

	public CachedResourceMap() {
		super();
		this.cachedDao = new InMemoryResourceDao();
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {

		String interpretation = refP.getInterpretation();
		if (!interpretation.equals(INTERPRETATION_RESOURCE_REF)) {
			String msg = MessageFormat.format(
					"{0} is not an Resource reference as its interpretation is not {1} it is {2}", //$NON-NLS-1$
					refP.getLocator(), INTERPRETATION_RESOURCE_REF, interpretation);
			throw new StrolchException(msg);
		}

		if (refP.getUom().equals(UOM_NONE)) {
			String msg = MessageFormat.format("{0} is not an Resource reference as its UOM is not set to a type!", //$NON-NLS-1$
					refP.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	protected ResourceDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getResourceDao(tx);
	}

	@Override
	public ResourceDao getCachedDao() {
		return this.cachedDao;
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ResourceQuery query, ResourceVisitor<U> resourceVisitor) {
		return getCachedDao().doQuery(query, resourceVisitor);
	}
}
