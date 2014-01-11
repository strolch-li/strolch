package li.strolch.agent.impl;

import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Resource;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class CachedResourceMap extends CachedElementMap<Resource> implements ResourceMap {

	@Override
	protected ResourceDao getDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getResourceDao(tx);
	}
}
