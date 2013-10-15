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
package ch.eitchnet.xmlpers.api;

import java.io.File;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.xmlpers.impl.PathBuilder;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.util.FilenameUtility;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class MetadataDao {

	private static final Logger logger = LoggerFactory.getLogger(MetadataDao.class);

	private final PersistenceTransaction tx;
	private final PathBuilder pathBuilder;
	private final boolean verbose;

	public MetadataDao(PathBuilder pathBuilder, PersistenceTransaction tx, boolean verbose) {
		this.tx = tx;
		this.pathBuilder = pathBuilder;
		this.verbose = verbose;
	}

	public Set<String> queryTypeSet(ObjectRef parentRef) {
		assertNotClosed(this.tx);
		assertNotIdRef(parentRef);

		File queryPath = parentRef.getPath(this.pathBuilder);
		Set<String> keySet = queryTypeSet(queryPath);

		if (this.verbose) {
			String msg = "Found {0} types for {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, keySet.size(), parentRef.getName());
			logger.info(msg);
		}

		return keySet;
	}

	public Set<String> queryKeySet(ObjectRef parentRef) {
		assertNotClosed(this.tx);
		assertNotRootRef(parentRef);
		assertNotIdRef(parentRef);

		File queryPath = parentRef.getPath(this.pathBuilder);
		Set<String> keySet = queryKeySet(queryPath);

		if (this.verbose) {
			String msg = "Found {0} objects for {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, keySet.size(), parentRef.getName());
			logger.info(msg);
		}

		return keySet;
	}

	public long queryTypeSize(ObjectRef parentRef) {
		assertNotClosed(this.tx);
		assertNotRootRef(parentRef);
		assertNotIdRef(parentRef);

		File queryPath = parentRef.getPath(this.pathBuilder);
		long numberOfFiles = queryTypeSize(queryPath);

		if (this.verbose) {
			String msg = "Found {0} types for {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, numberOfFiles, parentRef.getName());
			logger.info(msg);
		}

		return numberOfFiles;
	}

	public long querySize(ObjectRef parentRef) {
		assertNotClosed(this.tx);

		File queryPath = parentRef.getPath(this.pathBuilder);
		long numberOfFiles = querySize(queryPath);

		if (this.verbose) {
			String msg = "Found {0} objects for {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, numberOfFiles, parentRef.getName());
			logger.info(msg);
		}

		return numberOfFiles;
	}

	/**
	 * Returns the types, i.e. directories in the given query path
	 * 
	 * @param queryPath
	 *            the path for which the types should be gathered
	 * 
	 * @return a set of types in the given query path
	 */
	private Set<String> queryTypeSet(File queryPath) {
		if (!queryPath.exists())
			return Collections.emptySet();

		if (!queryPath.isDirectory()) {
			String msg = "The path is not a directory, thus can not query type set for it: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, queryPath.getAbsolutePath());
			throw new IllegalArgumentException(msg);
		}

		Set<String> keySet = new HashSet<String>();
		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {
			if (subTypeFile.isFile()) {
				String filename = subTypeFile.getName();
				String id = FilenameUtility.getId(filename);
				keySet.add(id);
			}
		}

		return keySet;
	}

	/**
	 * Returns the ids of all objects in the given query path, i.e. the id part of all the files in the given query path
	 * 
	 * @param queryPath
	 *            the path for which the ids should be gathered
	 * 
	 * @return a set of ids for the objects in the given query path
	 */
	private Set<String> queryKeySet(File queryPath) {
		if (!queryPath.exists())
			return Collections.emptySet();

		if (!queryPath.isDirectory()) {
			String msg = "The path is not a directory, thus can not query key set for it: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, queryPath.getAbsolutePath());
			throw new IllegalArgumentException(msg);
		}

		Set<String> keySet = new HashSet<String>();

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {
			if (subTypeFile.isFile()) {
				String filename = subTypeFile.getName();
				String id = FilenameUtility.getId(filename);
				keySet.add(id);
			}
		}

		return keySet;
	}

	/**
	 * Returns the number of all types, i.e. directories in the given query path
	 * 
	 * @param queryPath
	 *            the path in which to count the types
	 * 
	 * @return the number of types in the given query path
	 */
	private long queryTypeSize(File queryPath) {
		if (!queryPath.exists())
			return 0L;

		if (!queryPath.isDirectory()) {
			String msg = "The path is not a directory, thus can not query type size for it: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, queryPath.getAbsolutePath());
			throw new IllegalArgumentException(msg);
		}

		long numberOfFiles = 0l;

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isDirectory())
				numberOfFiles++;
		}
		return numberOfFiles;
	}

	/**
	 * Returns the number of all objects in the given query path
	 * 
	 * @param queryPath
	 *            the path in which to count the objects
	 * 
	 * @return the number of objects in the given query path
	 */
	private long querySize(File queryPath) {
		if (!queryPath.exists())
			return 0L;

		if (!queryPath.isDirectory()) {
			String msg = "The path is not a directory, thus can not query key size for it: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, queryPath.getAbsolutePath());
			throw new IllegalArgumentException(msg);
		}

		long numberOfFiles = 0l;

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isFile())
				numberOfFiles++;
		}
		return numberOfFiles;
	}

	private void assertNotClosed(PersistenceTransaction tx) {
		if (!tx.isOpen()) {
			String msg = "Transaction has been closed and thus no operation can be performed!"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
	}

	private void assertNotIdRef(ObjectRef objectRef) {
		if (objectRef.isLeaf()) {
			String msg = "IdRef not allowed: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName());
			throw new IllegalArgumentException(msg);
		}
	}

	private void assertNotRootRef(ObjectRef objectRef) {
		if (objectRef.isRoot()) {
			String msg = "RootRef not allowed: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName());
			throw new IllegalArgumentException(msg);
		}
	}
}
