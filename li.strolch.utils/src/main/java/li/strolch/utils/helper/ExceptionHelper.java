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

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ExceptionHelper {

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
		return t.getClass().getName() + ": " + t.getMessage();
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
		if (t.getCause() == null)
			return getExceptionMessage(t);

		String root = getExceptionMessageWithCauses(t.getCause());
		return getExceptionMessage(t) + "\n" + root;
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
		StringWriter stringWriter = new StringWriter();
		PrintWriter writer = new PrintWriter(stringWriter);
		t.printStackTrace(writer);
		return stringWriter.toString();
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
		if (t.getCause() == null)
			return getExceptionMessage(t);

		String root = formatExceptionMessage(t.getCause());
		return getExceptionMessage(t) + "\ncause: " + root;
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
}
