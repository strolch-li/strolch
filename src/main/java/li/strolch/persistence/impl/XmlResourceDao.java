package li.strolch.persistence.impl;

import li.strolch.model.Resource;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class XmlResourceDao extends AbstractDao<Resource> implements ResourceDao {

	private static final String CLASS_TYPE = Resource.class.getName();

	protected XmlResourceDao(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassType() {
		return CLASS_TYPE;
	}
}
