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
package li.strolch.utils.objectfilter;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

import java.util.List;

import org.hamcrest.MatcherAssert;
import org.junit.Test;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
@SuppressWarnings("nls")
public class ObjectFilterTest {

	@Test
	public void shouldAdd() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj, myObj);

		testAssertions(filter, 1, 1, 1, 0, 0);
	}

	@Test
	public void shouldUpdate() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.update(myObj, myObj);

		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldRemove() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj, myObj);

		testAssertions(filter, 1, 1, 0, 0, 1);
	}

	@Test
	public void shouldAddUpdateRemoveDifferentObjects() {

		Object objToAdd = new Object();
		Object objToUpdate = new Object();
		Object objToRemove = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(objToAdd, objToAdd);
		filter.update(objToUpdate, objToUpdate);
		filter.remove(objToRemove, objToRemove);

		testAssertions(filter, 3, 1, 1, 1, 1);
	}

	@Test
	public void shouldAddUpdateRemoveSameObject() {

		Object objToAddUpdateRemove = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(objToAddUpdateRemove, objToAddUpdateRemove);
		filter.update(objToAddUpdateRemove, objToAddUpdateRemove);
		filter.remove(objToAddUpdateRemove, objToAddUpdateRemove);

		testAssertions(filter, 0, 1, 0, 0, 0);
	}

	@Test
	public void shouldNotAddTwice() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj, myObj);

		try {
			filter.add(myObj, myObj);
			fail("Should have failed adding twice!");
		} catch (RuntimeException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("Stale State exception: Invalid + after +"));
		}

		testAssertions(filter, 1, 1, 1, 0, 0);
	}

	@Test
	public void shouldNotRemoveTwice() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj, myObj);

		try {
			filter.remove(myObj, myObj);
			fail("Should have failed removing twice!");
		} catch (RuntimeException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("Stale State exception: Invalid - after -"));
		}

		testAssertions(filter, 1, 1, 0, 0, 1);
	}

	@Test
	public void shouldAcceptUpdateTwice() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.update(myObj, myObj);
		filter.update(myObj, myObj);
		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldStillBeAddWithUpdate() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj, myObj);
		filter.update(myObj, myObj);
		filter.update(myObj, myObj);
		testAssertions(filter, 1, 1, 1, 0, 0);
	}

	@Test
	public void shouldNotAcceptAddAfterModify() {
		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.update(myObj, myObj);

		try {
			filter.add(myObj, myObj);
			fail("Should have failed add after modify");
		} catch (RuntimeException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("Stale State exception: Invalid + after +="));
		}

		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldAcceptAddAfterRemove() {
		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj, myObj);
		filter.add(myObj, myObj);

		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldNotAcceptModifyAfterRemove() {
		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj, myObj);

		try {
			filter.update(myObj, myObj);
			fail("Should have failed modify after remove");
		} catch (RuntimeException e) {
			MatcherAssert.assertThat(e.getMessage(), containsString("Stale State exception: Invalid += after -"));
		}

		testAssertions(filter, 1, 1, 0, 0, 1);
	}

	@Test
	public void shouldReplaceOnAddAfterRemove() {

		TestObject obj1 = new TestObject(1);
		TestObject obj2 = new TestObject(1);
		assertEquals("Test objects are not equal!", obj1, obj2);

		ObjectFilter filter = new ObjectFilter();
		filter.remove(Object.class.getName(), obj1, obj1);
		filter.add(Object.class.getName(), obj2, obj2);

		testAssertions(filter, 1, 1, 0, 1, 0);

		List<Object> updated = filter.getUpdated(Object.class.getName());
		Object updatedObj = updated.get(0);
		String msg = "registered object is not the last operation's object";
		assertEquals(msg, obj2, updatedObj);
	}

	@Test
	public void shouldReplaceOnUpdateAfterAdd() {

		TestObject obj1 = new TestObject(1);
		TestObject obj2 = new TestObject(1);
		assertEquals("Test objects are not equal!", obj1, obj2);

		ObjectFilter filter = new ObjectFilter();
		filter.add(Object.class.getName(), obj1, obj1);
		filter.update(Object.class.getName(), obj2, obj2);

		testAssertions(filter, 1, 1, 1, 0, 0);

		List<Object> added = filter.getAdded(Object.class.getName());
		Object addedObj = added.get(0);
		String msg = "registered object is not the last operation's object";
		assertEquals(msg, obj2, addedObj);
	}

	@Test
	public void shouldReplaceOnUpdateAfterUpdate() {

		TestObject obj1 = new TestObject(1);
		TestObject obj2 = new TestObject(1);
		assertEquals("Test objects are not equal!", obj1, obj2);

		ObjectFilter filter = new ObjectFilter();
		filter.update(Object.class.getName(), obj1, obj1);
		filter.update(Object.class.getName(), obj2, obj2);

		testAssertions(filter, 1, 1, 0, 1, 0);

		List<Object> updated = filter.getUpdated(Object.class.getName());
		Object updatedObj = updated.get(0);
		String msg = "registered object is not the last operation's object";
		assertEquals(msg, obj2, updatedObj);
	}

	@Test
	public void shouldReplaceOnRemoveAfterModify() {

		TestObject obj1 = new TestObject(1);
		TestObject obj2 = new TestObject(1);
		assertEquals("Test objects are not equal!", obj1, obj2);

		ObjectFilter filter = new ObjectFilter();
		filter.update(Object.class.getName(), obj1, obj1);
		filter.remove(Object.class.getName(), obj2, obj2);

		testAssertions(filter, 1, 1, 0, 0, 1);

		List<Object> removed = filter.getRemoved(Object.class.getName());
		Object removedObj = removed.get(0);
		String msg = "registered object is not the last operation's object";
		assertEquals(msg, obj2, removedObj);
	}

	@Test
	public void shouldRemoveAfterAddAndRemove() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj, myObj);
		filter.remove(myObj, myObj);
		testAssertions(filter, 0, 1, 0, 0, 0);
	}

	@Test
	public void shouldClear() {

		Object myObj1 = new Object();
		Object myObj2 = new Object();
		Object myObj3 = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj1, myObj1);
		filter.update(myObj2, myObj2);
		filter.remove(myObj3, myObj3);

		filter.clearCache();

		testAssertions(filter, 0, 0, 0, 0, 0);
	}

	@Test
	public void shouldGetAll() {

		Object myObj1 = new Object();
		Object myObj2 = new Object();
		Object myObj3 = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj1, myObj1);
		filter.update(myObj2, myObj2);
		filter.remove(myObj3, myObj3);

		testAssertions(filter, 3, 1, 1, 1, 1);

		List<Object> all = filter.getAll(Object.class.getName());
		assertEquals(3, all.size());
		assertTrue(all.contains(myObj1));
		assertTrue(all.contains(myObj2));
		assertTrue(all.contains(myObj3));
	}

	@Test
	public void shouldGetAdded() {

		Object myObj1 = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj1, myObj1);

		testAssertions(filter, 1, 1, 1, 0, 0);

		List<Object> list = filter.getAdded(Object.class.getName());
		assertEquals(1, list.size());
		assertTrue(list.contains(myObj1));
	}

	@Test
	public void shouldGetUpdated() {

		Object myObj1 = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.update(myObj1, myObj1);

		testAssertions(filter, 1, 1, 0, 1, 0);

		List<Object> list = filter.getUpdated(Object.class.getName());
		assertEquals(1, list.size());
		assertTrue(list.contains(myObj1));
	}

	@Test
	public void shouldGetRemoved() {

		Object myObj1 = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj1, myObj1);

		testAssertions(filter, 1, 1, 0, 0, 1);

		List<Object> list = filter.getRemoved(Object.class.getName());
		assertEquals(1, list.size());
		assertTrue(list.contains(myObj1));
	}

	private void testAssertions(ObjectFilter filter, int size, int sizeKeySet, int added, int updated, int removed) {
		assertEquals(size, filter.sizeCache());
		assertEquals(sizeKeySet, filter.sizeKeySet());

		List<Object> addedList = filter.getAdded(Object.class.getName());
		assertEquals(added, addedList.size());

		List<Object> updatedList = filter.getUpdated(Object.class.getName());
		assertEquals(updated, updatedList.size());

		List<Object> removedList = filter.getRemoved(Object.class.getName());
		assertEquals(removed, removedList.size());
	}

	private class TestObject {
		private final int id;

		public TestObject(int id) {
			this.id = id;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getOuterType().hashCode();
			result = prime * result + this.id;
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			TestObject other = (TestObject) obj;
			if (!getOuterType().equals(other.getOuterType()))
				return false;
			return this.id == other.id;
		}

		private ObjectFilterTest getOuterType() {
			return ObjectFilterTest.this;
		}
	}
}
