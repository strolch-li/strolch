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
package li.strolch.xmlpers.test;

import static li.strolch.utils.helper.SystemHelper.isLinux;
import static li.strolch.utils.helper.SystemHelper.isWindows;

import java.io.File;
import java.util.Properties;

import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.SystemHelper;
import li.strolch.xmlpers.api.IoMode;
import li.strolch.xmlpers.api.PersistenceConstants;
import li.strolch.xmlpers.api.PersistenceManager;
import li.strolch.xmlpers.api.PersistenceManagerLoader;
import li.strolch.xmlpers.test.impl.BookContextFactory;
import li.strolch.xmlpers.test.impl.MyModelContextFactory;
import li.strolch.xmlpers.test.impl.TestConstants;
import li.strolch.xmlpers.test.model.Book;
import li.strolch.xmlpers.test.model.MyModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractPersistenceTest {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractPersistenceTest.class);

	protected PersistenceManager persistenceManager;

	@SuppressWarnings("nls")
	protected static void cleanPath(String path) {

		File file = new File(path).getAbsoluteFile();
		File parent = file.getParentFile();
		if (isWindows()) {
			if (!parent.getAbsolutePath().endsWith("\\target\\db"))
				throw new RuntimeException("Bad parent! Must be \\target\\db: " + parent);
		} else {
			if (!parent.getAbsolutePath().endsWith("/target/db"))
				throw new RuntimeException("Bad parent! Must be /target/db: " + parent);
		}

		if (!parent.exists() && !parent.mkdirs())
			throw new RuntimeException("Failed to create path " + parent);

		if (file.exists() && file.isDirectory())
			if (!FileHelper.deleteFiles(file.listFiles(), true))
				throw new RuntimeException("Could not clean up path " + file.getAbsolutePath());

		if (!file.exists() && !file.mkdirs())
			throw new RuntimeException("Failed to create path " + file);

		File domFile = new File(file, IoMode.DOM.name());
		if (!domFile.mkdir())
			throw new RuntimeException("Failed to create path " + domFile);

		File saxFile = new File(file, IoMode.SAX.name());
		if (!saxFile.mkdir())
			throw new RuntimeException("Failed to create path " + saxFile);
	}

	protected void setup(Properties properties) {
		properties.setProperty(PersistenceConstants.PROP_VERBOSE, "true"); //$NON-NLS-1$
		this.persistenceManager = PersistenceManagerLoader.load(properties);
		this.persistenceManager.getCtxFactory()
				.registerPersistenceContextFactory(MyModel.class, TestConstants.TYPE_RES, new MyModelContextFactory());
		this.persistenceManager.getCtxFactory()
				.registerPersistenceContextFactory(Book.class, TestConstants.TYPE_BOOK, new BookContextFactory());
	}
}
