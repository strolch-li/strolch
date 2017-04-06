/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.Tags;
import li.strolch.model.query.ActivityQueryVisitor;
import li.strolch.model.query.ActivityStateSelection;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlActivityQueryVisitor extends PostgreSqlQueryVisitor implements ActivityQueryVisitor {

	/**
	 * @param fields
	 */
	public PostgreSqlActivityQueryVisitor(String fields) {
		super(fields);
	}

	@Override
	protected String getClassName() {
		return Tags.ACTIVITY;
	}

	@Override
	protected String getTableName() {
		return PostgreSqlActivityDao.ACTIVITIES;
	}

	@Override
	public void visit(ActivityStateSelection selection) {
		this.sb.append(this.indent);
		this.sb.append("state = ?::order_state\n");
		this.values.add(selection.getState().name());
	}
}
