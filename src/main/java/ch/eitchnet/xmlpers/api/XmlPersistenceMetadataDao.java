/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.xmlpers
 *
 * ch.eitchnet.java.xmlpers is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.xmlpers is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.xmlpers.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.xmlpers.api;

import java.util.Set;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface XmlPersistenceMetadataDao {

	public Set<String> queryTypeSet();

	public Set<String> queryTypeSet(String type);

	public Set<String> queryKeySet(String type);

	public Set<String> queryKeySet(String type, String subType);

	public long queryTypeSize();

	public long querySubTypeSize(String type);

	public long querySize(String type);

	public long querySize(String type, String subType);
}
