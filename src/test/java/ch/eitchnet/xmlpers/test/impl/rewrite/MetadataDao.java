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

import static ch.eitchnet.xmlpers.test.impl.rewrite.AssertionUtil.assertHasType;

import java.io.File;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.xmlpers.api.PersistenceContext;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class MetadataDao {

	private static final Logger logger = LoggerFactory.getLogger(MetadataDao.class);

	private final DefaultPersistenceTransaction tx;
	private final FileDao fileDao;
	private final boolean verbose;

	public MetadataDao(DefaultPersistenceTransaction tx, FileDao fileDao, boolean verbose) {
		this.tx = tx;
		this.fileDao = fileDao;
		this.verbose = verbose;
	}

	public Set<String> queryTypeSet(PersistenceContext<?> ctx) {
		assertNotClosed();
		assertHasNoSubType(ctx);

		File queryPath = this.fileDao.getPath(ctx);
		Set<String> keySet = queryTypeSet(queryPath);

		if (this.verbose) {
			String msg;
			if (!ctx.hasType()) {
				msg = "Found {0} types"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, keySet.size());
			} else {
				msg = "Found {0} subTypes of type {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, keySet.size(), ctx.getType());
			}

			logger.info(msg);
		}

		return keySet;
	}

	private void assertHasNoSubType(PersistenceContext<?> ctx) {
		if (ctx.hasSubType()) {
			throw new RuntimeException("Illegal query: sub type may not be set!"); //$NON-NLS-1$
		}
	}

	public Set<String> queryKeySet(PersistenceContext<?> ctx) {
		assertNotClosed();
		assertHasType(ctx);

		File queryPath = this.fileDao.getPath(ctx);
		Set<String> keySet = queryKeySet(queryPath);

		if (this.verbose) {
			String msg;
			if (!ctx.hasSubType()) {
				msg = "Found {0} objects of type {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, keySet.size(), ctx.getType());
			} else {
				msg = "Found {0} objects of type {1} and subtType {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, keySet.size(), ctx.getType(), ctx.getSubType());
			}

			logger.info(msg);
		}

		return keySet;
	}

	public long queryTypeSize(PersistenceContext<?> ctx) {
		assertNotClosed();
		assertHasNoSubType(ctx);

		File queryPath = this.fileDao.getPath(ctx);
		long numberOfFiles = queryTypeSize(queryPath);

		if (this.verbose) {
			String msg;
			if (!ctx.hasType()) {
				msg = "Found {0} types"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, numberOfFiles);
			} else {
				msg = "Found {0} subTypes of type {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, numberOfFiles, ctx.getType());
			}

			logger.info(msg);
		}

		return numberOfFiles;
	}

	public long querySize(PersistenceContext<?> ctx) {
		assertNotClosed();
		assertHasType(ctx);

		File queryPath = this.fileDao.getPath(ctx);
		long numberOfFiles = querySize(queryPath);

		if (this.verbose) {
			String msg;
			if (!ctx.hasSubType()) {
				msg = "Found {0} objects of type {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, numberOfFiles, ctx.getType());
			} else {
				msg = "Found {0} objects of type {1} and subtType {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, numberOfFiles, ctx.getType(), ctx.getSubType());
			}

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

	private void assertNotClosed() {
		if (!this.tx.isOpen()) {
			String msg = "Transaction has been closed and thus no operation can be performed!"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
	}
}
