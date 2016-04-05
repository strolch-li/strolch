package li.strolch.rest.endpoint;

import javax.ws.rs.QueryParam;

public class QueryData {

	@QueryParam("realmName")
	private String realmName;

	@QueryParam("draw")
	private int draw;

	@QueryParam("pageSize")
	private int pageSize;

	@QueryParam("page")
	private int page;

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

	public int getDraw() {
		return this.draw;
	}

	public void setDraw(int draw) {
		this.draw = draw;
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

	public int getPageSize() {
		return this.pageSize;
	}

	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}

	public int getPage() {
		return this.page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public String getQuery() {
		return this.query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
}
