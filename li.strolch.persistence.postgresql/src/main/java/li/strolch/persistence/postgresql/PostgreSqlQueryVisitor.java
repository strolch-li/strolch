/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.persistence.postgresql;

import static li.strolch.persistence.postgresql.PostgreSqlHelper.toSql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import li.strolch.model.query.AndSelection;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.NotSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.ParameterBagSelection;
import li.strolch.model.query.ParameterBagSelection.NullParameterBagSelection;
import li.strolch.model.query.ParameterSelection.AnyTypeParameterSelection;
import li.strolch.model.query.ParameterSelection.BooleanParameterSelection;
import li.strolch.model.query.ParameterSelection.DateParameterSelection;
import li.strolch.model.query.ParameterSelection.DateRangeParameterSelection;
import li.strolch.model.query.ParameterSelection.DurationParameterSelection;
import li.strolch.model.query.ParameterSelection.FloatListParameterSelection;
import li.strolch.model.query.ParameterSelection.FloatParameterSelection;
import li.strolch.model.query.ParameterSelection.IntegerListParameterSelection;
import li.strolch.model.query.ParameterSelection.IntegerParameterSelection;
import li.strolch.model.query.ParameterSelection.LongListParameterSelection;
import li.strolch.model.query.ParameterSelection.LongParameterSelection;
import li.strolch.model.query.ParameterSelection.NullParameterSelection;
import li.strolch.model.query.ParameterSelection.StringListParameterSelection;
import li.strolch.model.query.ParameterSelection.StringParameterSelection;
import li.strolch.model.query.ParameterSelectionVisitor;
import li.strolch.model.query.Selection;
import li.strolch.model.query.StrolchRootElementSelectionVisitor;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.model.query.ordering.OrderById;
import li.strolch.model.query.ordering.OrderByName;
import li.strolch.model.query.ordering.OrderByParameter;
import li.strolch.model.query.ordering.StrolchQueryOrderingVisitor;
import li.strolch.utils.StringMatchMode;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class PostgreSqlQueryVisitor
		implements StrolchRootElementSelectionVisitor, ParameterSelectionVisitor, StrolchQueryOrderingVisitor {

	protected StringBuilder sql;
	protected StringBuilder sb;
	protected String ordering;
	protected String type;
	protected List<Object> values;
	protected boolean any;
	protected String indent;
	private String sqlAsString;

	public PostgreSqlQueryVisitor(String fields) {
		this.indent = "";
		this.sql = new StringBuilder();
		this.sb = new StringBuilder();
		this.values = new ArrayList<>();

		this.sql.append("SELECT ");
		this.sql.append(fields);
		this.sql.append("\nFROM\n");
		this.sql.append("  ");
		this.sql.append(getTableName());
		this.indent = "  ";
	}

	public String getSql() {
		if (this.sqlAsString != null)
			return this.sqlAsString;

		this.sql.append("\nWHERE\n");
		this.sql.append(this.indent);

		if (this.any) {
			this.sql.append("type = ?");
			appendOrdering();
			this.sqlAsString = this.sql.toString();
			return this.sqlAsString;
		}

		this.sql.append("type = ? AND\n");
		this.sql.append(this.sb.toString());

		appendOrdering();

		this.sqlAsString = this.sql.toString();
		return this.sqlAsString;
	}

	private void appendOrdering() {
		if (StringHelper.isNotEmpty(this.ordering)) {
			this.sql.append("\n");
			this.sql.append(this.ordering);
		}
	}

	/**
	 * @return the any
	 */
	public boolean isAny() {
		return this.any;
	}

	public String getType() {
		return this.type;
	}

	public void validate() {
		DBC.INTERIM.assertNotEmpty("No navigation was set!", this.type);
	}

	protected abstract String getClassName();

	protected abstract String getTableName();

	@Override
	public void visit(StrolchTypeNavigation navigation) {
		this.type = navigation.getType();
	}

	@Override
	public void visit(IdSelection selection) {
		this.sb.append(this.indent);
		List<String> ids = selection.getIds();
		if (ids.isEmpty())
			return;
		int size = ids.size();
		if (size == 1) {
			this.sb.append("id = ?\n");
			this.values.add(ids.get(0));
		} else {
			this.sb.append("id IN (");
			Iterator<String> iter = ids.iterator();
			while (iter.hasNext()) {
				String id = iter.next();
				this.sb.append("?");
				this.values.add(id);
				if (iter.hasNext())
					this.sb.append(", ");
			}
			this.sb.append(" )\n");
		}
	}

	@Override
	public void visit(NameSelection selection) {
		this.sb.append(this.indent);
		String name = selection.getName();
		StringMatchMode mm = selection.getMatchMode();
		this.sb.append(toSql("name", this.indent, mm, this.values, name));
	}

	@Override
	public void visitAny() {
		this.any = true;
	}

	@Override
	public void visitAnd(AndSelection andSelection) {
		this.sb.append(this.indent);
		List<Selection> selections = andSelection.getSelections();
		this.sb.append("( \n");
		Iterator<Selection> iter = selections.iterator();
		String indent = this.indent;
		this.indent += "  ";
		while (iter.hasNext()) {
			Selection selection = iter.next();
			selection.accept(this);
			if (iter.hasNext()) {
				this.sb.append(indent);
				this.sb.append("AND\n");
			}
		}
		this.indent = indent;
		this.sb.append(this.indent);
		this.sb.append(")\n");
	}

	@Override
	public void visitOr(OrSelection orSelection) {
		this.sb.append(this.indent);
		List<Selection> selections = orSelection.getSelections();
		this.sb.append("( \n");
		Iterator<Selection> iter = selections.iterator();
		String indent = this.indent;
		this.indent += "  ";
		while (iter.hasNext()) {
			Selection selection = iter.next();
			selection.accept(this);
			if (iter.hasNext()) {
				this.sb.append(indent);
				this.sb.append("OR\n");
			}
		}
		this.indent = indent;
		this.sb.append(this.indent);
		this.sb.append(")\n");
	}

	@Override
	public void visitNot(NotSelection notSelection) {
		this.sb.append(this.indent);
		List<Selection> selections = notSelection.getSelections();
		this.sb.append("NOT ( \n");
		Iterator<Selection> iter = selections.iterator();
		String indent = this.indent;
		this.indent += "  ";
		while (iter.hasNext()) {
			Selection selection = iter.next();
			selection.accept(this);
			if (iter.hasNext()) {
				this.sb.append(indent);
				this.sb.append("AND\n");
			}
		}
		this.indent = indent;
		this.sb.append(this.indent);
		this.sb.append(")\n");
	}

	private void xpath(String bagKey, String paramKey, String paramValue) {
		String xpath = "CAST(XPATH('//${className}/ParameterBag[@Id=\"${bagKey}\"]/Parameter[@Id=\"${paramKey}\" and @Value=\"${paramValue}\"]', asxml) AS TEXT[]) != '{}'\n";
		this.sb.append(this.indent);
		xpath = xpath.replace("${className}", getClassName());
		xpath = xpath.replace("${bagKey}", bagKey);
		xpath = xpath.replace("${paramKey}", paramKey);
		xpath = xpath.replace("${paramValue}", paramValue);
		this.sb.append(xpath);
	}

	private void xpath(String bagKey, String paramKey, String value, StringMatchMode matchMode) {
		String xpath = "XPATH('//${className}/ParameterBag[@Id=\"${bagKey}\"]/Parameter[@Id=\"${paramKey}\"]/@Value', asxml))::TEXT AS content";
		xpath = xpath.replace("${className}", getClassName());
		xpath = xpath.replace("${bagKey}", bagKey);
		xpath = xpath.replace("${paramKey}", paramKey);

		this.sb.append(this.indent);
		this.sb.append("id IN (\n");
		this.sb.append(this.indent);
		this.sb.append("  SELECT id\n");
		this.sb.append(this.indent);
		this.sb.append("  FROM (\n");
		this.sb.append(this.indent);
		this.sb.append("    SELECT id, UNNEST(");
		this.sb.append(xpath);
		this.sb.append("\n");
		this.sb.append(this.indent);
		this.sb.append("FROM ");
		this.sb.append(getTableName());
		this.sb.append("\n");
		this.sb.append(this.indent);
		this.sb.append(") AS alias\n");
		this.sb.append(this.indent);
		this.sb.append("WHERE ");

		if (matchMode.isEquals()) {
			if (matchMode.isCaseSensitve()) {
				this.sb.append("content = ?\n");
			} else {
				this.sb.append("content ILIKE ?\n");
			}
		} else {
			value = "%" + value + "%";
			if (matchMode.isCaseSensitve()) {
				this.sb.append("content LIKE ?\n");
			} else {
				this.sb.append("content ILIKE ?\n");
			}
		}

		this.sb.append(this.indent);
		this.sb.append(")\n");

		this.values.add(value);
	}

	@Override
	public void visit(StringParameterSelection sel) {
		xpath(sel.getBagKey(), sel.getParamKey(), sel.getValue(), sel.getMatchMode());
	}

	@Override
	public void visit(IntegerParameterSelection selection) {
		xpath(selection.getBagKey(), selection.getParamKey(), selection.getValue().toString());
	}

	@Override
	public void visit(BooleanParameterSelection selection) {
		xpath(selection.getBagKey(), selection.getParamKey(), selection.getValue().toString());
	}

	@Override
	public void visit(LongParameterSelection selection) {
		xpath(selection.getBagKey(), selection.getParamKey(), selection.getValue().toString());
	}

	@Override
	public void visit(FloatParameterSelection selection) {
		xpath(selection.getBagKey(), selection.getParamKey(), selection.getValue().toString());
	}

	@Override
	public void visit(DateParameterSelection selection) {
		xpath(selection.getBagKey(), selection.getParamKey(),
				ISO8601FormatFactory.getInstance().formatDate(selection.getValue()));
	}

	@Override
	public void visit(DurationParameterSelection selection) {
		xpath(selection.getBagKey(), selection.getParamKey(),
				ISO8601FormatFactory.getInstance().formatDuration(selection.getValue()));
	}

	@Override
	public void visit(NullParameterSelection selection) {
		String xpath = "CAST(XPATH('//${className}/ParameterBag[@Id=\"${bagKey}\"]/Parameter[@Id=\"${paramKey}\"]', asxml) AS text[]) = '{}'\n";
		this.sb.append(this.indent);
		xpath = xpath.replace("${className}", getClassName());
		xpath = xpath.replace("${bagKey}", selection.getBagKey());
		xpath = xpath.replace("${paramKey}", selection.getParamKey());
		this.sb.append(xpath);
	}

	@Override
	public void visit(ParameterBagSelection selection) {
		String xpath = "CAST(XPATH('//${className}/ParameterBag[@Id=\"${bagKey}\"]', asxml) AS text[]) != '{}'\n";
		this.sb.append(this.indent);
		xpath = xpath.replace("${className}", getClassName());
		xpath = xpath.replace("${bagKey}", selection.getBagKey());
		this.sb.append(xpath);
	}

	@Override
	public void visit(NullParameterBagSelection selection) {
		String xpath = "CAST(XPATH('//${className}/ParameterBag[@Id=\"${bagKey}\"]', asxml) AS text[]) = '{}'\n";
		this.sb.append(this.indent);
		xpath = xpath.replace("${className}", getClassName());
		xpath = xpath.replace("${bagKey}", selection.getBagKey());
		this.sb.append(xpath);
	}

	@Override
	public void visit(DateRangeParameterSelection selection) {
		throw new UnsupportedOperationException("Not yet supported!");
	}

	@Override
	public void visit(StringListParameterSelection selection) {
		throw new UnsupportedOperationException("Not yet supported!");
	}

	@Override
	public void visit(IntegerListParameterSelection selection) {
		throw new UnsupportedOperationException("Not yet supported!");
	}

	@Override
	public void visit(FloatListParameterSelection selection) {
		throw new UnsupportedOperationException("Not yet supported!");
	}

	@Override
	public void visit(LongListParameterSelection selection) {
		throw new UnsupportedOperationException("Not yet supported!");
	}

	@Override
	public PostgreSqlQueryVisitor visit(OrderById ordering) {
		if (ordering.isAscending())
			this.ordering = "ORDER BY id";
		else
			this.ordering = "ORDER BY id DESC";

		return this;
	}

	@Override
	public PostgreSqlQueryVisitor visit(OrderByName ordering) {
		if (ordering.isAscending())
			this.ordering = "ORDER BY name";
		else
			this.ordering = "ORDER BY name DESC";

		return this;
	}

	@Override
	public PostgreSqlQueryVisitor visit(OrderByParameter ordering) {
		throw new UnsupportedOperationException("Not yet supported!");
	}

	@Override
	public void visit(AnyTypeParameterSelection sel) {
		xpath(sel.getBagKey(), sel.getParamKey(), sel.getValue(), sel.getMatchMode());
	}

	/**
	 * @param ps
	 * @throws SQLException
	 */
	public void setValues(PreparedStatement ps) throws SQLException {
		if (this.any) {
			ps.setString(1, this.type);
			return;
		}

		ps.setString(1, this.type);
		for (int i = 0; i < this.values.size(); i++) {
			ps.setObject(i + 2, this.values.get(i));
		}
	}
}
