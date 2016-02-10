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
package ch.eitchnet.xmlpers.util;

import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.objref.ObjectRef;

public class AssertionUtil {

	public static void assertNotNull(Object object) {
		if (object == null)
			throw new RuntimeException("Object may not be null!"); //$NON-NLS-1$
	}

	public static void assertObjectRead(PersistenceContext<?> context) {
		if (context.getObject() == null) {
			String msg = "Failed to read object with for {0}"; //$NON-NLS-1$
			ObjectRef objectRef = context.getObjectRef();
			msg = MessageFormat.format(msg, objectRef.getName());
			throw new RuntimeException(msg);
		}
	}

	public static void assertIsIdRef(ObjectRef objectRef) {
		if (!objectRef.isLeaf()) {
			String msg = "Expected IdRef not {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName());
			throw new IllegalArgumentException(msg);
		}
	}

	public static void assertIsNotIdRef(ObjectRef objectRef) {
		if (objectRef.isLeaf()) {
			String msg = "IdRef not expected {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName());
			throw new IllegalArgumentException(msg);
		}
	}

	public static void assertIsNotRootRef(ObjectRef objectRef) {
		if (objectRef.isRoot()) {
			String msg = "RootRef not expected {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName());
			throw new IllegalArgumentException(msg);
		}
	}
}
