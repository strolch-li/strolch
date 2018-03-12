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
package li.strolch.model.query.ordering;

import li.strolch.model.ParameterBag;
import li.strolch.model.parameter.Parameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class OrderByParameter extends StrolchQueryOrdering {

	private String bagKey;
	private String paramKey;

	/**
	 * Creates this ordering with the given bagKey and paramKey with ascending order
	 * 
	 * @param bagKey
	 *            the {@link ParameterBag} in which to retrieve the {@link Parameter} for ordering
	 * @param paramKey
	 *            the {@link Parameter} with which to order
	 */
	public OrderByParameter(String bagKey, String paramKey) {
		super(true);
		this.bagKey = bagKey;
		this.paramKey = paramKey;
	}

	/**
	 * Creates this ordering with the given bagKey and paramKey and ascending order
	 * 
	 * @param ascending
	 *            true for ascending, false for descending order
	 * @param bagKey
	 *            the {@link ParameterBag} in which to retrieve the {@link Parameter} for ordering
	 * @param paramKey
	 *            the {@link Parameter} with which to order
	 */
	public OrderByParameter(boolean ascending, String bagKey, String paramKey) {
		super(ascending);
		this.bagKey = bagKey;
		this.paramKey = paramKey;
	}

	public String getBagKey() {
		return this.bagKey;
	}

	public String getParamKey() {
		return this.paramKey;
	}

	@Override
	public void accept(StrolchQueryOrderingVisitor visitor) {
		visitor.visit(this);
	}
}
