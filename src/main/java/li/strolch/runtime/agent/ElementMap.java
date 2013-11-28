/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.agent;

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
