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
package li.strolch.runtime.agent.api;

import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface ElementMap<T extends StrolchElement> {

	public boolean hasType(String type);

	public boolean hasElement(String type, String id);

	public T getBy(String type, String id);

	public List<T> getAllElements();

	public List<T> getElementsBy(String type);

	public Set<String> getTypes();

	public Set<String> getAllKeys();

	public Set<String> getKeysBy(String type);

	public void add(T element);

	public void update(T element);

	public void remove(T element);

}
