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
package li.strolch.model.query;

import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.parameter.Parameter;

/**
 * {@link ResourceQuery} is the user API to query {@link Resource Resources} in Strolch. The {@link Navigation} is used
 * to navigate to a type of resource on which any further {@link Selection Selections} will be performed. The
 * {@link ResourceVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query only the values of a {@link Parameter} instead of having to return all the elements and
 * then performing this transformation.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceQuery extends StrolchQuery<ResourceQueryVisitor> {

	/**
	 * @param navigation
	 * @param elementVisitor
	 */
	public ResourceQuery(Navigation navigation) {
		super(navigation);
	}

	@Override
	public ResourceQuery with(Selection selection) {
		super.with(selection);
		return this;
	}

	@Override
	public ResourceQuery not(Selection selection) {
		super.not(selection);
		return this;
	}

	@Override
	public ResourceQuery withAny() {
		super.withAny();
		return this;
	}

	public static ResourceQuery query(Navigation navigation) {
		return new ResourceQuery(navigation);
	}

	public static ResourceQuery query(String type) {
		return new ResourceQuery(new StrolchTypeNavigation(type));
	}
}
