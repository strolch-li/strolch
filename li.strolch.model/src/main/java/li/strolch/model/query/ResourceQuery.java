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
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.ordering.StrolchQueryOrdering;
import li.strolch.model.visitor.NoStrategyResourceVisitor;
import li.strolch.model.visitor.ResourceVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * <p>
 * {@link ResourceQuery} is the user API to query {@link Resource Resources} in Strolch. The {@link Navigation} is used
 * to navigate to a type of resource on which any further {@link Selection Selections} will be performed. The
 * {@link ResourceVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query only the values of a {@link Parameter} instead of having to return all the elements and
 * then performing this transformation.
 * </p>
 * 
 * <p>
 * The {@link ResourceVisitor} is intended for situations where the query result should not be {@link Resource} but some
 * other object type. For instance in a restful API, the result might have to be mapped to a POJO, thus using this
 * method can perform the mapping step for you
 * </p>
 * 
 * @param <U>
 *            defines the return type of this query. Depending on the user {@link ResourceVisitor} this query can return
 *            a {@link Resource}, or any type of object to which the visitor mapped the resource
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceQuery<U> extends StrolchElementQuery<ResourceQueryVisitor> {

	protected ResourceVisitor<U> resourceVisitor;
	protected StrolchQueryOrdering ordering;

	public ResourceQuery() {
		super();
	}

	public ResourceQuery(Navigation navigation) {
		super(navigation);
	}

	public ResourceVisitor<U> getResourceVisitor() {
		return this.resourceVisitor;
	}

	@Override
	public ResourceQuery<U> internal() {
		super.internal();
		return this;
	}

	public ResourceQuery<U> setResourceVisitor(ResourceVisitor<U> resourceVisitor) {
		DBC.PRE.assertNotNull("resourceVisitor", resourceVisitor);
		this.resourceVisitor = resourceVisitor;
		return this;
	}

	public StrolchQueryOrdering getOrdering() {
		return this.ordering;
	}

	public ResourceQuery<U> setOrdering(StrolchQueryOrdering ordering) {
		this.ordering = ordering;
		return this;
	}

	@Override
	public ResourceQuery<U> with(Selection selection) {
		super.with(selection);
		return this;
	}

	@Override
	public ResourceQuery<U> not(Selection selection) {
		super.not(selection);
		return this;
	}

	@Override
	public ResourceQuery<U> withAny() {
		super.withAny();
		return this;
	}

	@Override
	public void accept(ResourceQueryVisitor visitor) {
		DBC.PRE.assertNotNull("resourceVisitor", this.resourceVisitor);
		super.accept(visitor);
		if (this.ordering != null)
			this.ordering.accept(visitor);
	}

	public static ResourceQuery<Resource> query(String type) {
		return new ResourceQuery<Resource>(new StrolchTypeNavigation(type))
				.setResourceVisitor(new NoStrategyResourceVisitor());
	}

	public static ResourceQuery<Resource> query(String type, StrolchQueryOrdering ordering) {
		return new ResourceQuery<Resource>(new StrolchTypeNavigation(type))
				.setResourceVisitor(new NoStrategyResourceVisitor()).setOrdering(ordering);
	}

	public static <U> ResourceQuery<U> query(String type, ResourceVisitor<U> resourceVisitor) {
		return new ResourceQuery<U>(new StrolchTypeNavigation(type)).setResourceVisitor(resourceVisitor);
	}

	public static <U> ResourceQuery<U> query(String type, ResourceVisitor<U> resourceVisitor,
			StrolchQueryOrdering ordering) {
		return new ResourceQuery<U>(new StrolchTypeNavigation(type)).setResourceVisitor(resourceVisitor)
				.setOrdering(ordering);
	}
}
