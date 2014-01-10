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
package li.strolch.persistence.api;

import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface StrolchDao<T extends StrolchElement> {

	public Set<String> queryKeySet();

	public Set<String> queryKeySet(String type);

	public Set<String> queryTypes();

	public T queryBy(String type, String id);

	public List<T> queryAll();

	public List<T> queryAll(String type);

	public void save(T element);

	public void saveAll(List<T> elements);

	public void update(T element);

	public void updateAll(List<T> elements);

	public void remove(T element);
	
	public void removeAll(List<T> elements);
}
