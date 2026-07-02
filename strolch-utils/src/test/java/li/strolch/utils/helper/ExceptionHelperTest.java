/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ExceptionHelperTest {

	@Test
	public void shouldGetExceptionMsg() {

		Exception e = nestedException();
		assertEquals("RuntimeException: Third", ExceptionHelper.getExceptionMessage(e));
		assertEquals("""
				RuntimeException: Third
				RuntimeException: Second
				RuntimeException: First""", ExceptionHelper.getExceptionMessageWithCauses(e));
	}

	@Test
	public void shouldFormatException() {

		Exception e = nestedException();
		String formatException = ExceptionHelper.formatException(e);
		assertTrue(formatException.contains("java.lang.RuntimeException: First"));
		assertTrue(formatException.contains("java.lang.RuntimeException: Second"));
		assertTrue(formatException.contains("java.lang.RuntimeException: Third"));

		formatException = ExceptionHelper.formatExceptionMessage(e);
		assertEquals("""
				RuntimeException: Third
				cause: RuntimeException: Second
				cause: RuntimeException: First""", formatException);
	}

	private Exception nestedException() {
		try {
			try {
				try {
					throw new RuntimeException("First");
				} catch (Exception e) {
					throw new RuntimeException("Second", e);
				}
			} catch (Exception e) {
				throw new RuntimeException("Third", e);
			}
		} catch (Exception e) {
			return e;
		}
	}
}
