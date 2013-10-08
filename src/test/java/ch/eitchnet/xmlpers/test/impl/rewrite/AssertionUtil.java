package ch.eitchnet.xmlpers.test.impl.rewrite;

import java.text.MessageFormat;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.xmlpers.api.PersistenceContext;

public class AssertionUtil {

	public static void assertNotNull(Object object) {
		if (object == null)
			throw new RuntimeException("Object may not be null!"); //$NON-NLS-1$
	}

	public static void assertObjectRead(PersistenceContext<?> context) {
		if (context.getObject() == null) {
			String msg = "Failed to read object with for {0} / {1} / {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, context.getType(), context.getSubType(), context.getId());
			throw new RuntimeException(msg);
		}
	}

	public static void assertHasType(PersistenceContext<?> context) {
		if (StringHelper.isEmpty(context.getType()))
			throw new RuntimeException("Type is always needed on a persistence context!"); //$NON-NLS-1$
	}

	public static void assertHasId(PersistenceContext<?> context) {
		if (StringHelper.isEmpty(context.getId()))
			throw new RuntimeException("Id is not set on persistence context!"); //$NON-NLS-1$
	}

}
