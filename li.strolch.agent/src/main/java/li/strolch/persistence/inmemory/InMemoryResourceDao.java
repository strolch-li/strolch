package li.strolch.persistence.inmemory;

import java.util.List;

import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.runtime.query.inmemory.InMemoryQuery;
import li.strolch.runtime.query.inmemory.InMemoryResourceQueryVisitor;

public class InMemoryResourceDao extends InMemoryDao<Resource> implements ResourceDao {

	@Override
	public <U> List<U> doQuery(ResourceQuery resourceQuery, ResourceVisitor<U> resourceVisitor) {
		InMemoryResourceQueryVisitor visitor = new InMemoryResourceQueryVisitor();
		InMemoryQuery<Resource, U> query = visitor.visit(resourceQuery, resourceVisitor);
		return query.doQuery(this);
	}
}
