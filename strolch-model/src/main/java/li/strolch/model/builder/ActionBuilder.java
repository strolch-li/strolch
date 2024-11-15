/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.model.builder;

import li.strolch.model.activity.Action;
import li.strolch.utils.dbc.DBC;

import static li.strolch.model.builder.BuilderHelper.buildParamName;

public class ActionBuilder extends PolicyContainerBuilder<ActionBuilder> implements ActivityElementBuilder {

	private final ActivityBuilder builder;

	private String resourceId;
	private String resourceType;

	public ActionBuilder(String id, String type) {
		this(id, buildParamName(id), type);
	}

	public ActionBuilder(String id, String name, String type) {
		super(id, name, type);
		this.builder = null;
	}

	public ActionBuilder(ActivityBuilder builder, String id, String type) {
		this(builder, id, buildParamName(id), type);
	}

	public ActionBuilder(ActivityBuilder builder, String id, String name, String type) {
		super(id, name, type);
		this.builder = builder;
	}

	public ActivityBuilder endAction() {
		DBC.PRE.assertNotNull("Can not end, as not part of a builder context!", this.builder);
		return this.builder;
	}

	public ActionBuilder resource(String type, String id) {
		this.resourceType = type;
		this.resourceId = id;
		return this;
	}

	@Override
	public Action build() {
		Action action = new Action(getId(), getName(), getType());
		action.setResourceId(this.resourceId);
		action.setResourceType(this.resourceType);
		applyPolicyContainer(action);
		return action;
	}
}
