package li.strolch.rest.endpoint;

import java.util.Arrays;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import javax.ws.rs.QueryParam;

import ch.eitchnet.utils.helper.StringHelper;

public class QueryData {

	public static final String ID = "Id";
	public static final String NAME = "Name";
	public static final String TYPE = "Type";

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

	@QueryParam("queryBy")
	private String queryBy;

	@QueryParam("types")
	private String types;

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

	public String getQueryBy() {
		return this.queryBy;
	}

	public void setQueryBy(String queryBy) {
		this.queryBy = queryBy;
	}

	public String getTypes() {
		return this.types;
	}

	public void setTypes(String types) {
		this.types = types;
	}

	public Set<String> getQueryByNames() {
		return toSet(this.queryBy);
	}

	public Set<String> getTypesAsSet() {
		return toSet(this.types);
	}

	private Set<String> toSet(String value) {
		if (StringHelper.isEmpty(value))
			return Collections.emptySet();

		if (!value.contains(","))
			return Collections.singleton(value.trim());

		return Arrays.stream(value.split(",")).map(s -> s.trim()).collect(Collectors.toSet());
	}
}
