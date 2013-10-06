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

import java.util.Set;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class MetadataDao {

	private final DefaultPersistenceTransaction tx;
	private final FileDao fileDao;

	public MetadataDao(DefaultPersistenceTransaction tx, FileDao fileDao) {
		this.tx = tx;
		this.fileDao = fileDao;
	}

	public Set<String> queryTypeSet() {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	public Set<String> queryTypeSet(String type) {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	public Set<String> queryKeySet(String type) {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	public Set<String> queryKeySet(String type, String subType) {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	public long queryTypeSize() {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	public long querySubTypeSize(String type) {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	public long querySize(String type) {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	public long querySize(String type, String subType) {
		assertNotClosed();
		throw new UnsupportedOperationException("Not yet implemented!");
	}

	private void assertNotClosed() {
		if (this.tx.isClosed())
			throw new IllegalStateException("Transaction has been closed and thus no operation can be performed!");
	}
}
