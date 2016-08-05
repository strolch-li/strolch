/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.testbase.runtime;

import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.PARAM_STRING_ID;
import static li.strolch.model.ModelGenerator.createActivities;
import static li.strolch.model.ModelGenerator.createActivity;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;

@SuppressWarnings("nls")
public class ActivityModelTestRunner {

	private static final String ID = "@testActivity";
	private static final String NAME = "Test Activity";
	private static final String TYPE = "Produce";

	private RuntimeMock runtimeMock;
	private String realmName;
	private Certificate certificate;

	public ActivityModelTestRunner(RuntimeMock runtimeMock, String realmName) {
		this.runtimeMock = runtimeMock;
		this.realmName = realmName;

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".getBytes());
	}

	public void runCreateActivityTest() {

		// create
		Activity newActivity = createActivity("MyTestActivity", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getActivityMap().add(tx, newActivity);
			tx.commitOnClose();
		}
	}

	public void runQuerySizeTest() {

		// remove all
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getActivityMap().removeAll(tx, tx.getActivityMap().getAllElements(tx));
			tx.commitOnClose();
		}

		// create three activities
		Activity activity1 = createActivity("myTestActivity1", "Test Name", "QTestType1"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Activity activity2 = createActivity("myTestActivity2", "Test Name", "QTestType2"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Activity activity3 = createActivity("myTestActivity3", "Test Name", "QTestType3"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getActivityMap().add(tx, activity1);
			tx.getActivityMap().add(tx, activity2);
			tx.getActivityMap().add(tx, activity3);
			tx.commitOnClose();
		}

		// query size
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			long size = tx.getActivityMap().querySize(tx);
			assertEquals("Should have three objects", 3, size);

			size = tx.getActivityMap().querySize(tx, "QTestType1");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getActivityMap().querySize(tx, "QTestType2");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getActivityMap().querySize(tx, "QTestType3");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getActivityMap().querySize(tx, "NonExistingType");
			assertEquals("Should have zero objects of type 'NonExistingType'", 0, size);
		}
	}

	public void runCrudTests() {

		// create
		Activity newActivity = createActivity(ID, NAME, TYPE);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getActivityMap().add(tx, newActivity);
			tx.commitOnClose();
		}

		// read
		Activity readActivity = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			readActivity = tx.getActivityMap().getBy(tx, TYPE, ID);
		}
		assertNotNull("Should read Activity with id " + ID, readActivity);

		// update
		Parameter<String> sParam = readActivity.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!";
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getActivityMap().update(tx, readActivity);
			tx.commitOnClose();
		}

		// read updated
		Activity updatedActivity = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			updatedActivity = tx.getActivityMap().getBy(tx, TYPE, ID);
		}
		assertNotNull("Should read Activity with id " + ID, updatedActivity);
		if (this.runtimeMock.getRealm(this.realmName).getMode() != DataStoreMode.CACHED)
			assertFalse("Objects can't be the same reference after re-reading!", readActivity == updatedActivity);
		Parameter<String> updatedParam = readActivity.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getActivityMap().remove(tx, readActivity);
			tx.commitOnClose();
		}

		// fail to re-read
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			Activity activity = tx.getActivityMap().getBy(tx, TYPE, ID);
			assertNull("Should no read Activity with id " + ID, activity);
		}
	}

	public void runBulkOperationTests() {

		// create 15 activities
		List<Activity> activities = new ArrayList<>();
		activities.addAll(createActivities(activities.size(), 5, "@", "My Activity", "MyType1"));
		activities.addAll(createActivities(activities.size(), 5, "@", "Other Activity", "MyType2"));
		activities.addAll(createActivities(activities.size(), 5, "@", "Further Activity", "MyType3"));

		// sort them so we know which activity our objects are
		Comparator<Activity> comparator = new Comparator<Activity>() {
			@Override
			public int compare(Activity o1, Activity o2) {
				return o1.getId().compareTo(o2.getId());
			}
		};
		Collections.sort(activities, comparator);

		// first clear the map, so that we have a clean state
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			ActivityMap activityMap = tx.getActivityMap();
			activityMap.removeAll(tx, activityMap.getAllElements(tx));
			tx.commitOnClose();
		}

		{
			// make sure it is empty
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(0, activityMap.querySize(tx));
			}

			// now add some activities
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				tx.getActivityMap().addAll(tx, activities);
				tx.commitOnClose();
			}

			// make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(activities.size(), activityMap.querySize(tx));
				assertEquals(5, activityMap.querySize(tx, "MyType3"));
			}

			// now use the remove all by type
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				tx.getActivityMap().removeAllBy(tx, "MyType3");
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(activities.size() - 5, activityMap.querySize(tx));
				assertEquals(0, activityMap.querySize(tx, "MyType3"));
			}

			// now use the remove all
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				long removed = tx.getActivityMap().removeAll(tx);
				assertEquals(activities.size() - 5, removed);
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(0, activityMap.querySize(tx));
			}
		}

		// remove the version
		activities.forEach(t -> t.setVersion(null));

		// now add all again
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			tx.getActivityMap().addAll(tx, activities);
			tx.commitOnClose();
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			List<Activity> allActivities = tx.getActivityMap().getAllElements(tx);
			Collections.sort(allActivities, comparator);
			assertEquals(activities, allActivities);
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			ActivityMap activityMap = tx.getActivityMap();

			Set<String> types = activityMap.getTypes(tx);
			assertEquals(expectedTypes, types);

			Set<String> keySet = activityMap.getAllKeys(tx);
			assertEquals(15, keySet.size());

			for (String type : types) {
				Set<String> idsByType = activityMap.getKeysBy(tx, type);
				assertEquals(5, idsByType.size());

				List<Activity> activitiesByType = activityMap.getElementsBy(tx, type);
				assertEquals(5, activitiesByType.size());
			}
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			Activity activity = tx.getActivityMap().getBy(tx, "MyType1", "@00000001");
			assertNotNull(activity);
			activity = tx.getActivityMap().getBy(tx, "MyType2", "@00000006");
			assertNotNull(activity);
			activity = tx.getActivityMap().getBy(tx, "MyType3", "@00000011");
			assertNotNull(activity);
		}
	}
}
