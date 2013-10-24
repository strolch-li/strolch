package li.strolch.persistence.impl.dao.test;

import static li.strolch.testbase.model.ModelBuilder.BAG_ID;
import static li.strolch.testbase.model.ModelBuilder.PARAM_STRING_ID;
import static li.strolch.testbase.model.ModelBuilder.createResource;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;

import org.junit.Test;

public class XmlResourceDaoTest extends AbstractDaoImplTest {

	private static final String ID = "@testResource"; //$NON-NLS-1$
	private static final String NAME = "Test Resource"; //$NON-NLS-1$
	private static final String TYPE = "Box"; //$NON-NLS-1$

	@Test
	public void shouldCrud() {

		// create
		Resource newResource = createResource(ID, NAME, TYPE);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).save(newResource);
		}

		// read
		Resource readResource = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			readResource = persistenceHandler.getResourceDao(tx).queryBy(TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, readResource); //$NON-NLS-1$

		// update
		Parameter<String> sParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!"; //$NON-NLS-1$
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).update(readResource);
		}

		// read updated
		Resource updatedResource = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			updatedResource = persistenceHandler.getResourceDao(tx).queryBy(TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, updatedResource); //$NON-NLS-1$
		assertFalse("Objects can't be the same reference after re-reading!", readResource == updatedResource); //$NON-NLS-1$
		Parameter<String> updatedParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).remove(readResource);
		}

		// fail to re-read
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			Resource resource = persistenceHandler.getResourceDao(tx).queryBy(TYPE, ID);
			assertNull("Should no read Resource with id " + ID, resource); //$NON-NLS-1$
		}
	}
}
