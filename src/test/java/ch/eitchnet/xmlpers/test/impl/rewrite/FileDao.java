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

import java.io.File;
import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.impl.XmlPersistencePathBuilder;

public class FileDao {

	private static final Logger logger = LoggerFactory.getLogger(FileDao.class);

	private final boolean verbose;
	private final XmlPersistencePathBuilder pathBuilder;

	public FileDao(XmlPersistencePathBuilder pathBuilder, boolean verbose) {
		this.pathBuilder = pathBuilder;
		this.verbose = verbose;
	}

	File getPath(PersistenceContext<?> context) {
		return this.pathBuilder.getPath(context);
	}

	void performCreate(PersistenceContext<?> context) {
		File path = this.pathBuilder.getCreatePath(context);
		FileIo fileIo = new FileIo(path);
		fileIo.write(context);
	}

	void performRead(PersistenceContext<?> context) {
		File path = this.pathBuilder.getReadPath(context);
		if (path == null) {
			context.setObject(null);
		} else {
			FileIo fileIo = new FileIo(path);
			fileIo.read(context);
		}
	}

	void performUpdate(PersistenceContext<?> context) {
		File path = this.pathBuilder.getUpdatePath(context);
		FileIo fileIo = new FileIo(path);
		fileIo.write(context);
	}

	void performDelete(PersistenceContext<?> context) {
		File path = this.pathBuilder.getDeletePath(context);
		if (!path.delete()) {
			String msg = "Failed to delete file {0}"; //$NON-NLS-1$
			throw new RuntimeException(MessageFormat.format(msg, path.getAbsolutePath()));
		}

		deleteEmptyDirectory(path.getParentFile(), context);
	}

	private void deleteEmptyDirectory(File directoryPath, PersistenceContext<?> ctx) {
		if (!directoryPath.isDirectory()) {
			String msg = "The given path for deletion when empty is not a directory:{0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, directoryPath.getAbsolutePath());
			throw new IllegalArgumentException(msg);
		}
		if (directoryPath.list().length == 0) {
			if (!ctx.hasSubType()) {
				if (!directoryPath.delete()) {
					throw failedToDelete(directoryPath, ctx);
				}
				if (this.verbose) {
					String msg = "Deleted empty directory for type {0} at {1}"; //$NON-NLS-1$
					logger.info(MessageFormat.format(msg, ctx.getType(), directoryPath));
				}
			} else {

				if (!directoryPath.delete()) {
					throw failedToDelete(directoryPath, ctx);
				}
				if (this.verbose) {
					String msg = "Deleted empty directory for subType {0} of type {1} at {2}"; //$NON-NLS-1$
					logger.info(MessageFormat.format(msg, ctx.getSubType(), ctx.getType(), directoryPath));
				}

				File typePath = directoryPath.getParentFile();
				if (typePath.list().length == 0) {

					if (!typePath.delete()) {
						throw failedToDelete(typePath, ctx);
					}
					if (this.verbose) {
						String msg = "Deleted empty directory for type {0} at {1}"; //$NON-NLS-1$
						logger.info(MessageFormat.format(msg, ctx.getType(), typePath));
					}
				}
			}
		}
	}

	private XmlPersistenceException failedToDelete(File directoryPath, PersistenceContext<?> ctx) {
		String msg;
		if (ctx.hasSubType()) {
			msg = "Deletion of empty directory for {0} / {1} / {2} at {3} failed! Check file permissions!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, ctx.getType(), ctx.getSubType(), ctx.getId(),
					directoryPath.getAbsolutePath());
		} else {

			msg = "Deletion of empty directory for {0} / {1} / at {2} failed! Check file permissions!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, ctx.getType(), ctx.getId(), directoryPath.getAbsolutePath());
		}
		return new XmlPersistenceException(msg);
	}
}