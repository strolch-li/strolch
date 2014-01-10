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

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ElementMap<T extends StrolchElement> {

	public boolean hasType(StrolchTransaction tx, String type);

	public boolean hasElement(StrolchTransaction tx, String type, String id);

	public T getBy(StrolchTransaction tx, String type, String id);

	public List<T> getAllElements(StrolchTransaction tx);

	public List<T> getElementsBy(StrolchTransaction tx, String type);

	public Set<String> getTypes(StrolchTransaction tx);

	public Set<String> getAllKeys(StrolchTransaction tx);

	public Set<String> getKeysBy(StrolchTransaction tx, String type);

	public void add(StrolchTransaction tx, T element);

	public void update(StrolchTransaction tx, T element);

	public void remove(StrolchTransaction tx, T element);
}
