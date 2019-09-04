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
package li.strolch.xmlpers.api;

import java.io.File;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import li.strolch.xmlpers.impl.PathBuilder;
import li.strolch.xmlpers.objref.ObjectRef;
import li.strolch.xmlpers.util.FilenameUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
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

		this.tx.lock(parentRef);
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
		return queryKeySet(parentRef, false);
	}

	public Set<String> queryKeySet(ObjectRef parentRef, boolean reverse) {
		assertNotClosed(this.tx);
		assertNotRootRef(parentRef);
		assertNotIdRef(parentRef);

		this.tx.lock(parentRef);
		File queryPath = parentRef.getPath(this.pathBuilder);
		Set<String> keySet = queryKeySet(queryPath, reverse);

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

		this.tx.lock(parentRef);
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

		this.tx.lock(parentRef);
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
	 * 		the path for which the types should be gathered
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

		Set<String> keySet = new TreeSet<>();
		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {
			if (subTypeFile.isDirectory()) {
				String type = subTypeFile.getName();
				keySet.add(type);
			}
		}

		return keySet;
	}

	/**
	 * Returns the ids of all objects in the given query path, i.e. the id part of all the files in the given query
	 * path
	 *
	 * @param queryPath
	 * 		the path for which the ids should be gathered
	 *
	 * @return a set of ids for the objects in the given query path
	 */
	private Set<String> queryKeySet(File queryPath, boolean reverse) {
		if (!queryPath.exists())
			return Collections.emptySet();

		if (!queryPath.isDirectory()) {
			String msg = "The path is not a directory, thus can not query key set for it: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, queryPath.getAbsolutePath());
			throw new IllegalArgumentException(msg);
		}

		Comparator<String> comparator = reverse ? Comparator.reverseOrder() : Comparator.naturalOrder();
		Set<String> keySet = new TreeSet<>(comparator);

		File[] subTypeFiles = queryPath.listFiles();
		if (subTypeFiles == null) {
			String msg = "The path does not exist, thus can not query key set for it: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, queryPath.getAbsolutePath());
			throw new IllegalArgumentException(msg);
		}

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
	 * 		the path in which to count the types
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
	 * 		the path in which to count the objects
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
