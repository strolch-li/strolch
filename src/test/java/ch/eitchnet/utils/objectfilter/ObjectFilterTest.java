/*
 * Copyright (c) 2012
 *
 * This file is part of ???????????????
 *
 * ?????????????? is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ????????????????.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.utils.objectfilter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ObjectFilterTest {

	@Test
	public void shouldAdd() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj);

		testAssertions(filter, 1, 1, 1, 0, 0);
	}

	@Test
	public void shouldUpdate() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.update(myObj);

		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldRemove() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj);

		testAssertions(filter, 1, 1, 0, 0, 1);
	}

	@Test
	public void shouldAddUpdateRemoveDifferentObjects() {

		Object objToAdd = new Object();
		Object objToUpdate = new Object();
		Object objToRemove = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(objToAdd);
		filter.update(objToUpdate);
		filter.remove(objToRemove);

		testAssertions(filter, 3, 1, 1, 1, 1);
	}

	@Test
	public void shouldAddUpdateRemoveSameObject() {

		Object objToAddUpdateRemove = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(objToAddUpdateRemove);
		filter.update(objToAddUpdateRemove);
		filter.remove(objToAddUpdateRemove);

		testAssertions(filter, 0, 1, 0, 0, 0);
	}

	@Test
	public void shouldNotAddTwice() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj);

		try {
			filter.add(myObj);
			fail("Should have failed adding twice!");
		} catch (RuntimeException e) {
			assertEquals("Stale State exception: Invalid + after +", e.getMessage());
		}

		testAssertions(filter, 1, 1, 1, 0, 0);
	}

	@Test
	public void shouldNotRemoveTwice() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj);

		try {
			filter.remove(myObj);
			fail("Should have failed removing twice!");
		} catch (RuntimeException e) {
			assertEquals("Stale State exception: Invalid - after -", e.getMessage());
		}

		testAssertions(filter, 1, 1, 0, 0, 1);
	}

	@Test
	public void shouldAcceptUpdateTwice() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.update(myObj);
		filter.update(myObj);
		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldStillBeAddWithUpdate() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj);
		filter.update(myObj);
		filter.update(myObj);
		testAssertions(filter, 1, 1, 1, 0, 0);
	}

	@Test
	public void shouldNotAcceptAddAfterModify() {
		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.update(myObj);

		try {
			filter.add(myObj);
			fail("Should have failed add after modify");
		} catch (RuntimeException e) {
			assertEquals("Stale State exception: Invalid + after +=", e.getMessage());
		}

		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldAcceptAddAfterRemove() {
		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj);
		filter.add(myObj);

		testAssertions(filter, 1, 1, 0, 1, 0);
	}

	@Test
	public void shouldNotAcceptModifyAfterRemove() {
		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.remove(myObj);

		try {
			filter.update(myObj);
			fail("Should have failed modify after remove");
		} catch (RuntimeException e) {
			assertEquals("Stale State exception: Invalid += after -", e.getMessage());
		}

		testAssertions(filter, 1, 1, 0, 0, 1);
	}

	@Test
	public void shouldNotAcceptDifferentKeyForSameObject() {
		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj);

		try {
			filter.update("different_key", myObj);
			fail("Should have failed because of different key for already registered object");
		} catch (RuntimeException e) {
			String msg = "Invalid key provided for object with transaction ID -1 and operation MODIFY:  existing key is java.lang.Object, new key is different_key. Object may be present in the same filter instance only once, registered using one key only. Object";
			assertTrue("Encountered exception: " + e.getMessage(), e.getMessage().contains(msg));
		}

		testAssertions(filter, 1, 1, 1, 0, 0);
	}

	@Test
	public void shouldReplaceInstanceIfObjectIsEqual() {
		fail("Not yet implemented");
	}

	@Test
	public void shouldRemoveAfterAddAndRemove() {

		Object myObj = new Object();

		ObjectFilter filter = new ObjectFilter();
		filter.add(myObj);
		filter.remove(myObj);
		testAssertions(filter, 0, 1, 0, 0, 0);
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
}
