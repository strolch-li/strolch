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
		if (!objectRef.isRoot()) {
			String msg = "RootRef not expected {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, objectRef.getName());
			throw new IllegalArgumentException(msg);
		}
	}
}
