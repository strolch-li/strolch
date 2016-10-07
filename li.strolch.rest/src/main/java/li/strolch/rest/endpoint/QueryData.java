package li.strolch.rest.endpoint;

import javax.ws.rs.QueryParam;

public class QueryData {

	@QueryParam("realmName")
	private String realmName;

	@QueryParam("offset")
	private int offset;

	@QueryParam("limit")
	private int limit;

	@QueryParam("query")
	private String query;

	@QueryParam("orderBy")
	private String orderBy;

	@QueryParam("ascending")
	private boolean ascending;

	public String getRealmName() {
		return this.realmName;
	}

	public void setRealmName(String realmName) {
		this.realmName = realmName;
	}

	public String getOrderBy() {
		return this.orderBy;
	}

	public void setOrderBy(String orderBy) {
		this.orderBy = orderBy;
	}

	public boolean isAscending() {
		return this.ascending;
	}

	public void setAscending(boolean ascending) {
		this.ascending = ascending;
	}

	public int getOffset() {
		return this.offset;
	}

	public void setOffset(int offset) {
		this.offset = offset;
	}

	public int getLimit() {
		return this.limit;
	}

	public void setLimit(int limit) {
		this.limit = limit;
	}

	public String getQuery() {
		return this.query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
}
