package li.strolch.persistence.inmemory;

import java.util.List;

import li.strolch.model.Resource;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.runtime.query.inmemory.InMemoryQuery;
import li.strolch.runtime.query.inmemory.InMemoryResourceQueryVisitor;

public class InMemoryResourceDao extends InMemoryDao<Resource> implements ResourceDao {

	@Override
	public <U> List<U> doQuery(ResourceQuery<U> resourceQuery) {
		InMemoryResourceQueryVisitor visitor = new InMemoryResourceQueryVisitor();
		InMemoryQuery<Resource, U> query = visitor.visit(resourceQuery);
		return query.doQuery(this);
	}
}
