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
package li.strolch.utils.dbc;

import java.io.File;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collection;

import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public enum DBC {

	PRE,
	INTERIM,
	POST;

	public void fail(String msg) {
		String ex = "Assertion failed: {0}"; //$NON-NLS-1$
		ex = MessageFormat.format(ex, msg);
		throw new DbcException(ex);
	}

	public <T> void assertEquals(String msg, T value1, T value2) {
		if (value1 == null && value2 == null)
			return;

		if (value1 != null && value1.equals(value2))
			return;

		if (value2 != null && value2.equals(value1))
			return;

		String ex = "{0}: {1} != {2}"; //$NON-NLS-1$
		ex = MessageFormat.format(ex, msg, value1, value2);
		throw new DbcException(ex);
	}

	public <T> void assertEqualsIgnoreOrdering(String msg, Collection<T> value1, Collection<T> value2) {
		if (value1 == null && value2 == null)
			return;

		if (value1 != null && value2 != null && value1.containsAll(value2) && value2.containsAll(value1))
			return;

		String ex = "{0}: {1} != {2}"; //$NON-NLS-1$
		ex = MessageFormat.format(ex, msg, value1, value2);
		throw new DbcException(ex);
	}

	public <T> void assertNotEquals(String msg, T value1, T value2) {
		if (value1 != null && !value1.equals(value2))
			return;

		if (value2 != null && !value2.equals(value1))
			return;

		String ex = "{0}: {1} == {2}"; //$NON-NLS-1$
		ex = MessageFormat.format(ex, msg, value1, value2);
		throw new DbcException(ex);
	}

	public void assertTrue(String msg, boolean value) {
		if (!value) {
			String ex = "Expected true, but was false: {0}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertFalse(String msg, boolean value) {
		if (value) {
			String ex = "Expected false, but was true: {0}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertEmpty(String msg, String value) {
		if (!StringHelper.isEmpty(value)) {
			String ex = "{0}: Illegal non-empty value: {1}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg, value);
			throw new DbcException(ex);
		}
	}

	public void assertEmpty(String msg, Object[] array) {
		assertNotNull(msg, array);
		if (array.length != 0) {
			String ex = "{0}: Illegal non-empty value: {1}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg, Arrays.toString(array));
			throw new DbcException(ex);
		}
	}

	public void assertEmpty(String msg, Collection<?> collection) {
		assertNotNull(msg, collection);
		if (!collection.isEmpty()) {
			String ex = "{0}: Illegal non-empty value: {1}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg, collection.toString());
			throw new DbcException(ex);
		}
	}

	public void assertNotEmpty(String msg, String value) {
		if (StringHelper.isEmpty(value)) {
			String ex = "{0}: Illegal empty value"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertNotEmpty(String msg, Object[] array) {
		assertNotNull(msg, array);
		if (array.length == 0) {
			String ex = "{0}: Illegal empty value"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertNotEmpty(String msg, Collection<?> collection) {
		assertNotNull(msg, collection);
		if (collection.isEmpty()) {
			String ex = "{0}: Illegal empty value"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertNotNull(String msg, Object value) {
		if (value == null) {
			String ex = "{0}: Illegal null value"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertNull(String msg, Object value) {
		if (value != null) {
			String ex = "{0}: {1} != null"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg, value);
			throw new DbcException(ex);
		}
	}

	public void assertNotExists(String msg, File file) {
		if (file.exists()) {
			String ex = MessageFormat.format("Illegal situation as file ({0}) exists: {1}", file, msg); //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertExists(String msg, File file) {
		if (!file.exists()) {
			String ex = MessageFormat
					.format("Illegal situation as file ({0}) does not exist: {1}", file, msg); //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public static class DbcException extends RuntimeException {

		public DbcException(String message) {
			super(message);
		}
	}
}
