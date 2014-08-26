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

import java.util.ArrayList;
import java.util.List;

import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlHelper {

	public static String toSql(String indent, StringMatchMode mm, List<Object> values, String... query) {

		//       CS     EQ
		// 1.    x      x
		// 2.    x      o
		// 3.    o      x
		// 4.    o      o

		StringBuilder sb = new StringBuilder();
		if (mm.isCaseSensitve() && mm.isEquals()) {
			if (query.length == 1) {
				sb.append("name = ?\n");
				values.add(query[0]);
			} else {
				sb.append("name in ( ");
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
				sb.append("lower(name) = ?\n");
				values.add(query[0].toLowerCase());
			} else {
				sb.append("lower(name) in ( ");
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
				sb.append("name like ?\n");
				values.add("%" + query[0] + "%");
			} else {
				sb.append("(\n");
				for (int i = 0; i < query.length; i++) {
					sb.append(indent);
					sb.append("  ");
					sb.append("name like ?");
					values.add("%" + query[i] + "%");
					if (i < query.length - 1)
						sb.append(" or");
					sb.append("\n");
				}
				sb.append(")\n");
			}
		} else {
			if (query.length == 1) {
				sb.append("lower(name) like ?\n");
				values.add("%" + query[0].toLowerCase() + "%");
			} else {
				sb.append("(\n");
				for (int i = 0; i < query.length; i++) {
					sb.append(indent);
					sb.append("  ");
					sb.append("lower(name) like ?");
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
		String sql = toSql("  ", StringMatchMode.CONTAINS_CASE_INSENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("  ", StringMatchMode.CONTAINS_CASE_INSENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();

		sql = toSql("  ", StringMatchMode.CONTAINS_CASE_SENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("  ", StringMatchMode.CONTAINS_CASE_SENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();

		sql = toSql("  ", StringMatchMode.EQUALS_CASE_INSENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("  ", StringMatchMode.EQUALS_CASE_INSENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();

		sql = toSql("  ", StringMatchMode.EQUALS_CASE_SENSITIVE, values, "foo", "bar", "fub");
		System.out.println(sql);
		System.out.println();

		sql = toSql("  ", StringMatchMode.EQUALS_CASE_SENSITIVE, values, "foo");
		System.out.println(sql);
		System.out.println();
	}
}
