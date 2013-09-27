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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import java.util.List;
import java.util.Set;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public class ObjectDao<T> {
	
	public void add(PersistenceContext<T> context){}

	public void update(PersistenceContext<T> context){}

	public void remove(PersistenceContext<T> context){}

	public void removeById(PersistenceContext<T> context){}

	public void removeAll(PersistenceContext<T> context){}

	public T queryById(PersistenceContext<T> context){return null;}

	public List<T> queryAll(PersistenceContext<T> context){return null;}

	public Set<String> queryKeySet(PersistenceContext<T> context){return null;}

	public long querySize(PersistenceContext<T> context){return 0L;}

}
