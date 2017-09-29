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

import li.strolch.model.audit.*;
import li.strolch.model.query.StringSelection;
import li.strolch.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlAuditQueryVisitor implements AuditQueryVisitor {

	protected StringBuilder sql;
	protected StringBuilder sb;
	protected List<Object> values;

	protected long limit;

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
		if (this.sqlAsString != null)
			return this.sqlAsString;

		this.sql.append("\nwhere\n");
		this.sql.append(this.sb.toString());

		this.sql.append("ORDER BY date DESC\n");

		if (limit != 0)
			this.sql.append("LIMIT " + limit + "\n");

		this.sqlAsString = this.sql.toString();
		return this.sqlAsString;
	}

	@Override
	public void visit(AuditQuery<?> auditQuery) {
		ensureAnd();
		this.sb.append(this.indent);
		this.sb.append(PostgreSqlAuditDao.ELEMENT_TYPE);
		this.sb.append(" = ?\n");
		this.limit = auditQuery.getLimit();
		ensureAnd();
		this.values.add(auditQuery.getElementTypeSelection());
		PostgreSqlHelper.toSql(this.indent, this.sb, this.values, PostgreSqlAuditDao.DATE, auditQuery.getDateRange());
	}

	@Override
	public void visit(ElementSelection selection) {
		if (!selection.isElementSubTypesWildcard()) {
			StringSelection sel = selection.getElementSubTypeSelection();
			toSql(PostgreSqlAuditDao.ELEMENT_SUB_TYPE, sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isElementAccessedWildcard()) {
			StringSelection sel = selection.getElementAccessedSelection();
			toSql(PostgreSqlAuditDao.ELEMENT_ACCESSED, sel.getMatchMode(), sel.getValues());
		}
	}

	@Override
	public void visit(IdentitySelection selection) {
		if (selection.isWildcard())
			return;

		if (!selection.isFirstnameWildcard()) {
			StringSelection sel = selection.getFirstnameSelection();
			toSql(PostgreSqlAuditDao.FIRSTNAME, sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isLastnameWildcard()) {
			StringSelection sel = selection.getLastnameSelection();
			toSql(PostgreSqlAuditDao.LASTNAME, sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isUsernameWildcard()) {
			StringSelection sel = selection.getUsernameSelection();
			toSql(PostgreSqlAuditDao.USERNAME, sel.getMatchMode(), sel.getValues());
		}
	}

	@Override
	public void visit(ActionSelection selection) {
		if (!selection.isWildcardAction()) {
			StringSelection sel = selection.getActionSelection();
			toSql(PostgreSqlAuditDao.ACTION, sel.getMatchMode(), sel.getValues());
		}

		if (!selection.isWildcardActionType()) {

			List<AccessType> accessTypes = selection.getAccessTypes();
			ensureAnd();
			this.sb.append(this.indent);
			if (accessTypes.size() == 1) {
				this.sb.append(PostgreSqlAuditDao.ACCESS_TYPE + " = ?");
				this.sb.append(PostgreSqlAuditDao.ACCESS_TYPE_TYPE);
				this.sb.append("\n");
				this.values.add(accessTypes.get(0).name());
			} else {
				this.sb.append(PostgreSqlAuditDao.ACCESS_TYPE + " in (");
				for (int i = 0; i < accessTypes.size(); i++) {
					this.sb.append("?");
					this.sb.append(PostgreSqlAuditDao.ACCESS_TYPE_TYPE);
					this.values.add(accessTypes.get(i).name());
					if (i < accessTypes.size() - 1)
						this.sb.append(", ");
				}
				this.sb.append(" )\n");
				this.sb.append("\n");
			}
		}
	}

	private void toSql(String column, StringMatchMode mm, String[] values) {
		ensureAnd();
		this.sb.append(this.indent);
		this.sb.append(PostgreSqlHelper.toSql(column, this.indent, mm, this.values, values));
	}

	public void setValues(PreparedStatement ps) throws SQLException {
		for (int i = 0; i < this.values.size(); i++) {
			ps.setObject(i + 1, this.values.get(i));
		}
	}

	private void ensureAnd() {
		if (this.sb.length() > 0) {
			this.sb.append(this.indent);
			this.sb.append("and \n");
		}
	}
}
