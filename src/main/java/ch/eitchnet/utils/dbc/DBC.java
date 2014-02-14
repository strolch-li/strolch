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
package ch.eitchnet.utils.dbc;

import java.io.File;
import java.text.MessageFormat;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum DBC {

	PRE, INTERIM, POST;

	public void assertEquals(String msg, Object value1, Object value2) {
		if (value1 == null && value2 == null)
			return;

		if (value1 != null && value1.equals(value2))
			return;

		if (value2 != null && value2.equals(value1))
			return;

		String ex = "Values are not equal: {0}"; //$NON-NLS-1$
		ex = MessageFormat.format(ex, msg);
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

	public void assertNotEmpty(String msg, String value) {
		if (StringHelper.isEmpty(value)) {
			String ex = "Illegal empty value: {0}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertNotNull(String msg, Object value) {
		if (value == null) {
			String ex = "Illegal null value: {0}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public void assertNull(String msg, Object value) {
		if (value != null) {
			String ex = "Illegal situation as value is not null: {0}"; //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
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
			String ex = MessageFormat.format("Illegal situation as file ({0}) does not exist: {1}", file, msg); //$NON-NLS-1$
			ex = MessageFormat.format(ex, msg);
			throw new DbcException(ex);
		}
	}

	public class DbcException extends RuntimeException {
		private static final long serialVersionUID = 1L;

		public DbcException(String message) {
			super(message);
		}
	}
}
