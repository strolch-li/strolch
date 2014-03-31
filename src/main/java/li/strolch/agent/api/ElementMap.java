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
package li.strolch.agent.api;

import java.util.List;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ElementMap<T extends StrolchElement> {

	public boolean hasType(StrolchTransaction tx, String type);

	public boolean hasElement(StrolchTransaction tx, String type, String id);

	public long querySize(StrolchTransaction tx);

	public long querySize(StrolchTransaction tx, String type);

	public T getBy(StrolchTransaction tx, String type, String id);

	/**
	 * Returns the Element which is referenced by the given {@link StringParameter}. A reference {@link Parameter} must
	 * have its interpretation set to the element type being referenced e.g. s
	 * {@link StrolchConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to the element's type and the value is
	 * the id of the element
	 * 
	 * @param tx
	 *            the {@link StrolchTransaction} instance
	 * @param refP
	 *            the {@link StringParameter} which references an element
	 * @return the element found, or null if it does not exist
	 * 
	 * @throws StrolchException
	 *             if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	public T getBy(StrolchTransaction tx, StringParameter refP) throws StrolchException;

	public List<T> getAllElements(StrolchTransaction tx);

	public List<T> getElementsBy(StrolchTransaction tx, String type);

	public Set<String> getTypes(StrolchTransaction tx);

	public Set<String> getAllKeys(StrolchTransaction tx);

	public Set<String> getKeysBy(StrolchTransaction tx, String type);

	public void add(StrolchTransaction tx, T element);

	public void addAll(StrolchTransaction tx, List<T> elements);

	public T update(StrolchTransaction tx, T element);

	public List<T> updateAll(StrolchTransaction tx, List<T> elements);

	public void remove(StrolchTransaction tx, T element);

	public void removeAll(StrolchTransaction tx, List<T> elements);
}
