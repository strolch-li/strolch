package li.strolch.rest.endpoint;

import javax.ws.rs.QueryParam;

import li.strolch.runtime.StrolchConstants;

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

	@QueryParam("descending")
	private boolean descending;

	public void initializeUnsetFields() {
		if (this.realmName == null)
			this.realmName = StrolchConstants.DEFAULT_REALM;
		if (this.limit == 0)
			this.limit = 50;
		if (this.query == null)
			this.query = "";
	}

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

	public boolean isDescending() {
		return this.descending;
	}

	public void setDescending(boolean descending) {
		this.descending = descending;
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
