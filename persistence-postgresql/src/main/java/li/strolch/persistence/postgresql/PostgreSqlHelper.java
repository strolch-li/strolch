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

import java.sql.Date;
import java.util.ArrayList;
import java.util.List;

import li.strolch.utils.StringMatchMode;
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlHelper {

	public static void toSql(String indent, StringBuilder sb, List<Object> values, String column, DateRange dateRange) {

		// TODO handle inclusive/exclusive: between is inclusive

		if (dateRange.isDate()) {
			sb.append(indent);
			sb.append(column);
			sb.append(" = ?\n");
			values.add(new Date(dateRange.getFromDate().getTime()));
		} else if (dateRange.isBounded()) {
			sb.append(indent);
			sb.append(column);
			sb.append(" between ? and ?\n");
			values.add(new Date(dateRange.getFromDate().getTime()));
			values.add(new Date(dateRange.getToDate().getTime()));
		} else if (dateRange.isToBounded()) {
			sb.append(indent);
			sb.append(column);
			sb.append(" <= ?\n");
			values.add(new Date(dateRange.getToDate().getTime()));
		} else if (dateRange.isFromBounded()) {
			sb.append(indent);
			sb.append(column);
			sb.append(" >= ?\n");
			values.add(new Date(dateRange.getFromDate().getTime()));
		}
	}

	public static String toSql(String column, String indent, StringMatchMode mm, List<Object> values, String... query) {

		//       CS     EQ
		// 1.    x      x
		// 2.    x      o
		// 3.    o      x
		// 4.    o      o

		StringBuilder sb = new StringBuilder();
		if (mm.isCaseSensitve() && mm.isEquals()) {
			if (query.length == 1) {
				sb.append(column).append(" = ?\n");
				values.add(query[0]);
			} else {
				sb.append(column).append(" in ( ");
				for (int i = 0; i < query.length; i++) {
					sb.append("?");
					values.add(query[i]);
					if (i < query.length - 1)
						sb.append(", ");
				}
				sb.append(" )\n");
			}
		} else if (!mm.isCaseSensitve() && mm.isEquals()) {
			if (query.length == 1) {
				sb.append("lower(").append(column).append(") = ?\n");
				values.add(query[0].toLowerCase());
			} else {
				sb.append("lower(").append(column).append(") in ( ");
				for (int i = 0; i < query.length; i++) {
					sb.append("?");
					values.add(query[i].toLowerCase());
					if (i < query.length - 1)
						sb.append(", ");
				}
				sb.append(" )\n");
			}
		} else if (!mm.isEquals() && mm.isCaseSensitve()) {
			if (query.length == 1) {
				sb.append(column).append(" like ?\n");
				values.add("%" + query[0] + "%");
			} else {
				sb.append("(\n");
				for (int i = 0; i < query.length; i++) {
					sb.append(indent);
					sb.append("  ");
					sb.append(column).append(" like ?");
					values.add("%" + query[i] + "%");
					if (i < query.length - 1)
						sb.append(" or");
					sb.append("\n");
				}
				sb.append(")\n");
			}
		} else {
			if (query.length == 1) {
				sb.append("lower(").append(column).append(") like ?\n");
				values.add("%" + query[0].toLowerCase() + "%");
			} else {
				sb.append("(\n");
				for (int i = 0; i < query.length; i++) {
					sb.append(indent);
					sb.append("  ");
					sb.append("lower(").append(column).append(") like ?");
					values.add("%" + query[i].toLowerCase() + "%");
					if (i < query.length - 1)
						sb.append(" or");
					sb.append("\n");
				}
				sb.append(")\n");
			}
		}

		return sb.toString();
	}

	public static void main(String[] args) {
		ArrayList<Object> values = new ArrayList<>();
		String sql = toSql("name", "  ", StringMatchMode.CONTAINS_CASE_INSENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("name", "  ", StringMatchMode.CONTAINS_CASE_INSENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();

		sql = toSql("name", "  ", StringMatchMode.CONTAINS_CASE_SENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("name", "  ", StringMatchMode.CONTAINS_CASE_SENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();

		sql = toSql("name", "  ", StringMatchMode.EQUALS_CASE_INSENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("name", "  ", StringMatchMode.EQUALS_CASE_INSENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();

		sql = toSql("name", "  ", StringMatchMode.EQUALS_CASE_SENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("name", "  ", StringMatchMode.EQUALS_CASE_SENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();
	}
}
