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
import li.strolch.model.visitor.NoStrategyResourceVisitor;

/**
 * {@link ResourceQuery} is the user API to query {@link Resource Resources} in Strolch. The {@link Navigation} is used
 * to navigate to a type of resource on which any further {@link Selection Selections} will be performed. The
 * {@link ResourceVisitor} is used to transform the returned object into a domain specific object (if required). This
 * mechanism allows you to query only the values of a {@link Parameter} instead of having to return all the elements and
 * then performing this transformation.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceQuery<U> extends StrolchQuery<ResourceQueryVisitor> {

	private ResourceVisitor<U> elementVisitor;

	/**
	 * Create a new
	 * 
	 * @param navigation
	 * @param elementVisitor
	 */
	public ResourceQuery(Navigation navigation, ResourceVisitor<U> elementVisitor) {
		super(navigation);
		this.elementVisitor = elementVisitor;
	}

	/**
	 * @return the elementVisitor
	 */
	public ResourceVisitor<U> getElementVisitor() {
		return this.elementVisitor;
	}

	/**
	 * Returns an instance of {@link ResourceQuery} where the visitor used is the {@link NoStrategyResourceVisitor} thus
	 * returning the actual Resource, i.e. no transformation is performed
	 * 
	 * @param navigation
	 * @return
	 */
	public static ResourceQuery<Resource> query(Navigation navigation) {
		return new ResourceQuery<Resource>(navigation, new NoStrategyResourceVisitor());
	}

	/**
	 * Returns an instance of {@link ResourceQuery} where the visitor used is the {@link NoStrategyResourceVisitor} thus
	 * returning the actual Resource, i.e. no transformation is performed
	 * 
	 * @param type
	 *            the type of {@link Resource} to navigate to
	 * @return
	 */
	public static ResourceQuery<Resource> query(String type) {
		return new ResourceQuery<Resource>(new StrolchTypeNavigation(type), new NoStrategyResourceVisitor());
	}

	/**
	 * Returns an instance of {@link ResourceQuery} using the given {@link ResourceVisitor} thus performing the given
	 * transformation
	 * 
	 * @param type
	 *            the type of Order to navigate to
	 * @param resourceVisitor
	 *            the visitor to use for transformation
	 * @return
	 */
	public static <U> ResourceQuery<U> query(String type, ResourceVisitor<U> resourceVisitor) {
		return new ResourceQuery<U>(new StrolchTypeNavigation(type), resourceVisitor);
	}
}
