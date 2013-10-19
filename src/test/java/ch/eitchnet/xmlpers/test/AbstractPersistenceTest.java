package ch.eitchnet.xmlpers.test;

import java.io.File;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceManager;
import ch.eitchnet.xmlpers.api.PersistenceManagerLoader;
import ch.eitchnet.xmlpers.test.impl.BookContextFactory;
import ch.eitchnet.xmlpers.test.impl.ResourceContextFactory;
import ch.eitchnet.xmlpers.test.impl.TestConstants;
import ch.eitchnet.xmlpers.test.model.Book;
import ch.eitchnet.xmlpers.test.model.Resource;

public abstract class AbstractPersistenceTest {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractPersistenceTest.class);

	protected PersistenceManager persistenceManager;

	@SuppressWarnings("nls")
	protected static void cleanPath(String path) {

		File file = new File(path).getAbsoluteFile();
		File parent = file.getParentFile();
		if (!parent.getAbsolutePath().endsWith("/target/db"))
			throw new RuntimeException("Bad parent! Must be /target/db/: " + parent);
		if (!parent.exists() && !parent.mkdirs())
			throw new RuntimeException("Failed to create path " + parent);

		if (file.exists() && file.isDirectory())
			if (!FileHelper.deleteFiles(file.listFiles(), true))
				throw new RuntimeException("Could not clean up path " + file.getAbsolutePath());

		if (!file.exists() && !file.mkdir())
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
		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Resource.class,
				TestConstants.TYPE_RES, new ResourceContextFactory());
		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Book.class, TestConstants.TYPE_BOOK,
				new BookContextFactory());
	}
}
