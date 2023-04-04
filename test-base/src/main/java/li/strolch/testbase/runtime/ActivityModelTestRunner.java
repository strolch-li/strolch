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

import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.*;

import java.util.*;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;

@SuppressWarnings("nls")
public class ActivityModelTestRunner {

	private static final String ID = "@testActivity";
	private static final String NAME = "Test Activity";
	private static final String TYPE = "Produce";

	private final RuntimeMock runtimeMock;
	private final String realmName;
	private final Certificate certificate;

	public ActivityModelTestRunner(RuntimeMock runtimeMock, String realmName) {
		this.runtimeMock = runtimeMock;
		this.realmName = realmName;

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public void runCreateActivityTest() {

		// create
		Activity newActivity = createActivity("MyTestActivity", "Test Name", "TestType",
				TimeOrdering.SERIES);//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(newActivity);
			tx.commitOnClose();
		}
	}

	public void runQuerySizeTest() {

		// remove all
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getActivityMap().removeAll(tx, tx.getActivityMap().getAllElements(tx));
			tx.commitOnClose();
		}

		// create three activities
		Activity activity1 = createActivity("myTestActivity1", "Test Name", "QTestType1",
				TimeOrdering.SERIES);//$NON-NLS-2$ //$NON-NLS-3$
		Activity activity2 = createActivity("myTestActivity2", "Test Name", "QTestType2",
				TimeOrdering.SERIES);//$NON-NLS-2$ //$NON-NLS-3$
		Activity activity3 = createActivity("myTestActivity3", "Test Name", "QTestType3",
				TimeOrdering.SERIES);//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(activity1);
			tx.add(activity2);
			tx.add(activity3);
			tx.commitOnClose();
		}

		// query size
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
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
		Activity newActivity = createActivity(ID, NAME, TYPE, TimeOrdering.SERIES);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(newActivity);
			tx.commitOnClose();
		}

		// read
		Activity readActivity;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			readActivity = tx.getActivityBy(TYPE, ID);
		}
		assertNotNull("Should read Activity with id " + ID, readActivity);

		// update
		StringParameter sParam = readActivity.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!";
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.update(readActivity);
			tx.commitOnClose();
		}

		// read updated
		Activity updatedActivity;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			updatedActivity = tx.getActivityBy(TYPE, ID);
		}
		assertNotNull("Should read Activity with id " + ID, updatedActivity);
		if (this.runtimeMock.getRealm(this.realmName).getMode() != DataStoreMode.CACHED)
			assertNotSame("Objects can't be the same reference after re-reading!", readActivity, updatedActivity);
		StringParameter updatedParam = readActivity.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.remove(readActivity);
			tx.commitOnClose();
		}

		// fail to re-read
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			Activity activity = tx.getActivityBy(TYPE, ID);
			assertNull("Should not read Activity with id " + ID, activity);
		}
	}

	public void runBulkOperationTests() {

		// create 15 activities
		List<Activity> activities = new ArrayList<>();
		activities.addAll(createActivities(activities.size(), 5, "@", "My Activity", "MyType1", TimeOrdering.SERIES));
		activities
				.addAll(createActivities(activities.size(), 5, "@", "Other Activity", "MyType2", TimeOrdering.SERIES));
		activities.addAll(createActivities(activities.size(), 5, "@", "Further Activity", "MyType3",
				TimeOrdering.SERIES));

		// sort them so we know which activity our objects are
		Comparator<Activity> comparator = Comparator.comparing(AbstractStrolchElement::getId);
		activities.sort(comparator);

		// first clear the map, so that we have a clean state
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			ActivityMap activityMap = tx.getActivityMap();
			List<Activity> allElements = activityMap.getAllElements(tx);
			long removed = activityMap.removeAll(tx);
			assertEquals(allElements.size(), removed);
			assertEquals(0, activityMap.querySize(tx));
			tx.commitOnClose();
		}

		{
			// make sure it is empty
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(0, activityMap.querySize(tx));
			}

			// now add some activities
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				tx.getActivityMap().addAll(tx, activities);
				tx.commitOnClose();
			}

			// make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(activities.size(), activityMap.querySize(tx));
				assertEquals(5, activityMap.querySize(tx, "MyType3"));
			}

			// now use the remove all by type
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				tx.getActivityMap().removeAllBy(tx, "MyType3");
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(activities.size() - 5, activityMap.querySize(tx));
				assertEquals(0, activityMap.querySize(tx, "MyType3"));
			}

			// now use the remove all
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				long removed = tx.getActivityMap().removeAll(tx);
				assertEquals(activities.size() - 5, removed);
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ActivityMap activityMap = tx.getActivityMap();
				assertEquals(0, activityMap.querySize(tx));
			}
		}

		// remove the version
		activities.forEach(t -> t.setVersion(null));

		// now add all again
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getActivityMap().addAll(tx, activities);
			tx.commitOnClose();
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			List<Activity> allActivities = tx.getActivityMap().getAllElements(tx);
			allActivities.sort(comparator);
			assertEquals(activities, allActivities);
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
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

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			Activity activity = tx.getActivityBy("MyType1", "@00000001");
			assertNotNull(activity);
			activity = tx.getActivityBy("MyType2", "@00000006");
			assertNotNull(activity);
			activity = tx.getActivityBy("MyType3", "@00000011");
			assertNotNull(activity);
		}
	}
}
