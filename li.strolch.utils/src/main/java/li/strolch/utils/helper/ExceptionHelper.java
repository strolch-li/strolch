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
package li.strolch.utils.helper;

import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.XmlHelper.PROP_LINE_SEPARATOR;
import static li.strolch.utils.helper.XmlHelper.UNIX_LINE_SEP;

import java.io.PrintWriter;
import java.io.StringWriter;

import li.strolch.utils.RemoveCRFilterWriter;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ExceptionHelper {

	/**
	 * Returns the class name and method name of the caller
	 *
	 * @return the class name and method name of the caller
	 */
	public static String getCallerMethod() {
		return getCallerMethod(2);
	}

	public static String getCallerMethod(int depth) {
		// TODO change to StackWalker:
//		StackWalker walker = StackWalker.getInstance(StackWalker.Option.RETAIN_CLASS_REFERENCE);
//		walker.walk(frames -> frames.skip(1)
//				.map((StackWalker.StackFrame s) -> s.getDeclaringClass() + "." + s.getMethodName()).findFirst());

		StackTraceElement element = new Throwable().getStackTrace()[depth];
		return element.getClassName() + "." + element.getMethodName();
	}

	/**
	 * <p>
	 * Returns a message for the given {@link Throwable}
	 * </p>
	 *
	 * <p>
	 * A {@link NullPointerException} only has <code>null</code> as the message so this methods returns the class name
	 * in such a case
	 * </p>
	 *
	 * @param t
	 * 		the {@link Throwable}
	 *
	 * @return the exception as string
	 */
	public static String getExceptionMessage(Throwable t) {
		return getExceptionMessage(t, true);
	}

	/**
	 * <p>
	 * Returns a message for the given {@link Throwable}
	 * </p>
	 *
	 * <p>
	 * A {@link NullPointerException} only has <code>null</code> as the message so this methods returns the class name
	 * in such a case
	 * </p>
	 *
	 * @param t
	 * 		the {@link Throwable}
	 * @param withClassName
	 * 		if true, then exception class name is prepended to the exception message, if the exception message is null,
	 * 		then this param is ignored
	 *
	 * @return the exception as string
	 */
	public static String getExceptionMessage(Throwable t, boolean withClassName) {
		if (withClassName || isEmpty(t.getMessage()))
			return t.getClass().getName() + ": " + t.getMessage();
		return t.getMessage();
	}

	/**
	 * <p>
	 * Returns a message for the given {@link Throwable}
	 * </p>
	 *
	 * <p>
	 * A {@link NullPointerException} only has <code>null</code> as the message so this methods returns the class name
	 * in such a case
	 * </p>
	 *
	 * @param t
	 * 		the {@link Throwable}
	 *
	 * @return the exception as string
	 */
	public static String getExceptionMessageWithCauses(Throwable t) {
		return getExceptionMessageWithCauses(t, true);
	}

	/**
	 * <p>
	 * Returns a message for the given {@link Throwable}
	 * </p>
	 *
	 * <p>
	 * A {@link NullPointerException} only has <code>null</code> as the message so this methods returns the class name
	 * in such a case
	 * </p>
	 *
	 * @param t
	 * 		the {@link Throwable}
	 * @param withClassName
	 * 		if true, then exception class name is prepended to the exception message, if the exception message is null,
	 * 		then this param is ignored
	 *
	 * @return the exception as string
	 */
	public static String getExceptionMessageWithCauses(Throwable t, boolean withClassName) {
		if (t.getCause() == null)
			return getExceptionMessage(t, withClassName);

		String root = getExceptionMessageWithCauses(t.getCause(), withClassName);
		return getExceptionMessage(t, withClassName) + "\n" + root;
	}

	/**
	 * Formats the given {@link Throwable}'s stack trace to a string
	 *
	 * @param t
	 * 		the throwable for which the stack trace is to be formatted to string
	 *
	 * @return a string representation of the given {@link Throwable}'s stack trace
	 */
	public static String formatException(Throwable t) {
		String ls = System.getProperty(PROP_LINE_SEPARATOR);
		if (!ls.equals(UNIX_LINE_SEP))
			System.setProperty(PROP_LINE_SEPARATOR, UNIX_LINE_SEP);
		try {
			StringWriter stringWriter = new StringWriter();
			PrintWriter writer = new PrintWriter(new RemoveCRFilterWriter(stringWriter));
			t.printStackTrace(writer);
			return stringWriter.toString();
		} finally {
			if (!ls.equals(UNIX_LINE_SEP))
				System.setProperty(PROP_LINE_SEPARATOR, ls);
		}
	}

	/**
	 * Formats the given {@link Throwable}'s message including causes to a string
	 *
	 * @param t
	 * 		the throwable for which the messages are to be formatted to a string
	 *
	 * @return a string representation of the given {@link Throwable}'s messages including causes
	 */
	public static String formatExceptionMessage(Throwable t) {
		return formatExceptionMessage(t, true);
	}

	/**
	 * Formats the given {@link Throwable}'s message including causes to a string
	 *
	 * @param t
	 * 		the throwable for which the messages are to be formatted to a string
	 * @param withClassName
	 * 		if true, then exception class name is prepended to the exception message, if the exception message is null, *
	 * 		then this param is ignored
	 *
	 * @return a string representation of the given {@link Throwable}'s messages including causes
	 */
	public static String formatExceptionMessage(Throwable t, boolean withClassName) {
		if (t.getCause() == null)
			return getExceptionMessage(t, withClassName);

		String root = formatExceptionMessage(t.getCause(), withClassName);
		return getExceptionMessage(t, withClassName) + "\ncause: " + root;
	}

	/**
	 * Returns the root cause for the given {@link Throwable}
	 *
	 * @param throwable
	 * 		the {@link Throwable} for which to get the root cause
	 *
	 * @return the root cause of the given {@link Throwable}
	 */
	public static Throwable getRootCause(Throwable throwable) {
		Throwable t = throwable;
		while (t.getCause() != null) {
			t = t.getCause();
		}

		return t;
	}

	/**
	 * Walks the causes for the given {@link Throwable} and sees if the given cause exists
	 *
	 * @param throwable
	 * 		the {@link Throwable} for which to find the given cause type
	 *
	 * @return true if the cause was found, false if not
	 */
	public static boolean hasCause(Throwable throwable, Class<? extends Throwable> causeType) {
		Throwable t = throwable;
		while (t != null) {
			if (t.getClass() == causeType)
				return true;
			t = t.getCause();
		}

		return false;
	}
}
