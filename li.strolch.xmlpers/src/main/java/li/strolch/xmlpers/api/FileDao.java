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

import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.xmlpers.impl.PathBuilder;
import li.strolch.xmlpers.objref.ObjectRef;

public class FileDao {

	private static final Logger logger = LoggerFactory.getLogger(FileDao.class);

	private final PersistenceTransaction tx;
	private final boolean verbose;
	private final PathBuilder pathBuilder;

	public FileDao(PersistenceTransaction tx, PathBuilder pathBuilder, boolean verbose) {
		DBC.PRE.assertNotNull("TX must not be null!", tx);
		DBC.PRE.assertNotNull("pathBuilder must not be null!", pathBuilder);
		this.tx = tx;
		this.pathBuilder = pathBuilder;
		this.verbose = verbose;
	}

	private void assertIsIdRef(IoOperation ioOperation, ObjectRef objectRef) {
		if (!objectRef.isLeaf()) {
			String msg = "A {0} operation can only be performed with IdRefs!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, ioOperation);
			throw new XmlPersistenceException(msg);
		}
	}

	public <T> boolean exists(PersistenceContext<T> ctx) {
		ObjectRef objectRef = ctx.getObjectRef();
		assertIsIdRef(IoOperation.READ, objectRef);
		File path = objectRef.getPath(this.pathBuilder);
		return path.exists();
	}

	public <T> void performCreate(PersistenceContext<T> ctx) {
		ObjectRef objectRef = ctx.getObjectRef();
		assertIsIdRef(IoOperation.CREATE, objectRef);
		File path = objectRef.getPath(this.pathBuilder);
		logPath(IoOperation.CREATE, path, objectRef);
		assertPathNotExists(path, objectRef);
		createMissingParents(path, objectRef);
		FileIo fileIo = new FileIo(path);
		this.tx.getManager().getIoMode().write(ctx, fileIo);
	}

	public <T> void performRead(PersistenceContext<T> ctx) {
		ObjectRef objectRef = ctx.getObjectRef();
		assertIsIdRef(IoOperation.READ, objectRef);
		File path = objectRef.getPath(this.pathBuilder);
		if (!path.exists()) {
			ctx.setObject(null);
			return;
		}

		logPath(IoOperation.READ, path, objectRef);
		FileIo fileIo = new FileIo(path);
		this.tx.getManager().getIoMode().read(ctx, fileIo);
	}

	public <T> void performUpdate(PersistenceContext<T> ctx) {
		ObjectRef objectRef = ctx.getObjectRef();
		assertIsIdRef(IoOperation.UPDATE, objectRef);
		File path = objectRef.getPath(this.pathBuilder);
		logPath(IoOperation.UPDATE, path, objectRef);
		assertPathIsFileAndWritable(path, objectRef);
		FileIo fileIo = new FileIo(path);
		this.tx.getManager().getIoMode().write(ctx, fileIo);
	}

	public <T> void performDelete(PersistenceContext<T> ctx) {
		ObjectRef objectRef = ctx.getObjectRef();
		assertIsIdRef(IoOperation.DELETE, objectRef);
		File path = objectRef.getPath(this.pathBuilder);
		logPath(IoOperation.DELETE, path, objectRef);
		assertPathIsFileAndWritable(path, objectRef);
		if (!path.delete()) {
			String msg = "Failed to delete file {0}"; //$NON-NLS-1$
			throw new RuntimeException(MessageFormat.format(msg, path.getAbsolutePath()));
		}

		ObjectRef parentRef = objectRef.getParent(this.tx);
		deleteEmptyDirectories(parentRef);
	}

	private void deleteEmptyDirectories(ObjectRef objectRef) {

		// root can't be deleted
		if (objectRef.isRoot())
			return;

		if (objectRef.isLeaf()) {
			throw new IllegalArgumentException("IdRefs don't reference directories!"); //$NON-NLS-1$
		}

		objectRef.lock();

		try {

			File directoryPath = objectRef.getPath(this.pathBuilder);
			if (!directoryPath.getAbsolutePath().startsWith(this.pathBuilder.getRootPath().getAbsolutePath())) {
				String msg = "The path for {0} is invalid as not child of {1}"; //$NON-NLS-1$
				msg = MessageFormat
						.format(msg, directoryPath.getAbsolutePath(), this.pathBuilder.getRootPath().getAbsolutePath());
				throw new IllegalArgumentException(msg);
			}

			if (!directoryPath.isDirectory()) {
				String msg = "The path for {0} is not a directory: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, objectRef.getName(), directoryPath.getAbsolutePath());
				throw new IllegalArgumentException(msg);
			}
			String[] list = directoryPath.list();
			if (list == null) {
				String msg = "The path for {0} is not a directory: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, objectRef.getName(), directoryPath.getAbsolutePath());
				throw new IllegalArgumentException(msg);
			}

			// stop if empty
			if (list.length != 0)
				return;

			// delete
			if (!directoryPath.delete()) {
				String msg = "Deletion of empty directory for {0} at {1} failed! Check file permissions!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, objectRef.getName(), directoryPath.getAbsolutePath());
				throw new XmlPersistenceException(msg);
			}

			// log
			if (this.verbose) {
				String msg = "Deleted empty directory for {0} at {1}"; //$NON-NLS-1$
				logger.info(MessageFormat.format(msg, objectRef.getName(), directoryPath));
			}

			// recursively delete
			ObjectRef parent = objectRef.getParent(this.tx);
			deleteEmptyDirectories(parent);

		} finally {
			objectRef.unlock();
		}
	}

	private void logPath(IoOperation operation, File path, ObjectRef objectRef) {
		if (this.verbose) {
			String msg = "Path for operation {0} for {1} is at {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, operation, objectRef.getName(), path.getAbsolutePath());
			logger.info(msg);
		}
	}

	private void createMissingParents(File path, ObjectRef objectRef) {
		ObjectRef parentRef = objectRef.getParent(this.tx);
		parentRef.lock();
		try {
			File parentFile = parentRef.getPath(this.pathBuilder);
			if (!parentFile.exists() && !parentFile.mkdirs()) {
				String msg = "Could not create parent path for {0} at {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, objectRef.getName(), path.getAbsolutePath());
				throw new XmlPersistenceException(msg);
			}
		} finally {
			parentRef.unlock();
		}
	}

	private void assertPathIsFileAndWritable(File path, ObjectRef objectRef) {
		if (!path.exists()) {
			String msg = "Persistence unit does not exist for {0} at {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName(), path.getAbsolutePath());
			throw new XmlPersistenceException(msg);
		}

		if (!path.isFile() || !path.canWrite()) {
			String msg;
			msg = "Persistence unit is not a file or is not readable for {0} at {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName(), path.getAbsolutePath());
			throw new XmlPersistenceException(msg);
		}
	}

	private void assertPathNotExists(File path, ObjectRef objectRef) {
		if (path.exists()) {
			String msg = "Persistence unit already exists for {0} at {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName(), path.getAbsolutePath());
			throw new XmlPersistenceException(msg);
		}
	}
}