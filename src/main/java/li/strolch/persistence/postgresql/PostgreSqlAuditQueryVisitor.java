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

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.ActionSelection;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditQueryVisitor;
import li.strolch.model.audit.DateRangeSelection;
import li.strolch.model.audit.ElementSelection;
import li.strolch.model.audit.IdentitySelection;
import li.strolch.model.query.StringSelection;
import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlAuditQueryVisitor implements AuditQueryVisitor {

	protected StringBuilder sql;
	protected StringBuilder sb;
	protected List<Object> values;
	protected String indent;
	private String sqlAsString;

	/**
	 * @param fields
	 */
	public PostgreSqlAuditQueryVisitor(String fields) {
		this.indent = "";
		this.sql = new StringBuilder();
		this.sb = new StringBuilder();
		this.values = new ArrayList<>();

		this.sql.append("select ");
		this.sql.append(fields);
		this.sql.append("\nfrom\n");
		this.sql.append("  ");
		this.sql.append(PostgreSqlAuditDao.TABLE_NAME);
		this.indent = "  ";
	}

	public String getSql() {
		if (sqlAsString != null)
			return sqlAsString;

		this.sql.append("\nwhere\n");
		this.sql.append(this.indent);
		this.sql.append(this.sb.toString());
		sqlAsString = this.sql.toString();
		return sqlAsString;
	}

	@Override
	public void visit(ElementSelection selection) {
		if (selection.isWildcard())
			return;

		if (!selection.isElementsAccessedWildcard()) {
			StringSelection sel = selection.getElementAccessedSelection();
			toSql(sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isElementTypesWildcard()) {
			StringSelection sel = selection.getElementTypeSelection();
			toSql(sel.getMatchMode(), sel.getValues());
		}
	}

	private void toSql(StringMatchMode mm, String[] values) {
		this.sb.append(this.indent);
		this.sb.append(PostgreSqlHelper.toSql(this.indent, mm, this.values, values));
	}

	@Override
	public void visit(IdentitySelection selection) {
		if (selection.isWildcard())
			return;

		if (!selection.isFirstnameWildcard()) {
			StringSelection sel = selection.getFirstnameSelection();
			toSql(sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isLastnameWildcard()) {
			StringSelection sel = selection.getLastnameSelection();
			toSql(sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isUsernameWildcard()) {
			StringSelection sel = selection.getUsernameSelection();
			toSql(sel.getMatchMode(), sel.getValues());
		}
	}

	@Override
	public void visit(DateRangeSelection selection) {

		// TODO Auto-generated method stub

	}

	@Override
	public void visit(ActionSelection selection) {
		if (!selection.isWildcardAction()) {
			StringSelection sel = selection.getActionSelection();
			toSql(sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isWildcardActionType()) {

			AccessType[] accessTypes = selection.getAccessTypes();
			String[] query = new String[accessTypes.length];
			for (int i = 0; i < accessTypes.length; i++) {
				query[i] = accessTypes[i].name();
			}

			toSql(StringMatchMode.EQUALS_CASE_SENSITIVE, query);
		}
	}

	@Override
	public void visit(AuditQuery auditQuery) {
		// TODO Auto-generated method stub

	}

	/**
	 * @param ps
	 * @throws SQLException
	 */
	public void setValues(PreparedStatement ps) throws SQLException {
		for (int i = 0; i < this.values.size(); i++) {
			ps.setObject(i + 1, this.values.get(i));
		}
	}
}
