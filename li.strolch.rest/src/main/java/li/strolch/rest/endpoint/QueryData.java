package li.strolch.rest.endpoint;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.QueryParam;

import ch.eitchnet.utils.helper.StringHelper;

public class QueryData {

	public static final String ID = "Id";
	public static final String NAME = "Name";
	public static final String TYPE = "Type";

	@QueryParam("realm")
	private String realmName;

	@QueryParam("draw")
	private int draw;

	@QueryParam("orderBy")
	private String orderBy;

	@QueryParam("ascending")
	private boolean ascending;

	@QueryParam("pageSize")
	private int pageSize;

	@QueryParam("page")
	private int page;

	@QueryParam("query")
	private String query;

	@QueryParam("queryBy")
	private String queryBy;

	@QueryParam("types")
	private String type;

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

	public String getQueryBy() {
		return this.queryBy;
	}

	public void setQueryBy(String queryBy) {
		this.queryBy = queryBy;
	}

	public String getType() {
		return this.type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Set<String> getQueryByNames() {
		return toSet(this.queryBy);
	}

	public Set<String> getTypes() {
		return toSet(this.type);
	}

	private Set<String> toSet(String value) {
		if (StringHelper.isEmpty(value))
			return Collections.emptySet();

		if (!value.contains(","))
			return Collections.singleton(value);

		return new HashSet<>(Arrays.asList(value.split(",")));
	}
}
