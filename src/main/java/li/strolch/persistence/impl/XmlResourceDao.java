package li.strolch.persistence.impl;

import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class XmlResourceDao extends AbstractDao<Resource> implements ResourceDao {

	protected XmlResourceDao(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassType() {
		return Tags.RESOURCE;
	}
}
