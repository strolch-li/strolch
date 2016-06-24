package ch.eitchnet.utils.helper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ExceptionHelperTest {

	@Test
	public void shouldGetExceptionMsg() {

		Exception e = nestedException();
		assertEquals("Third", ExceptionHelper.getExceptionMessage(e));
		assertEquals("Third\nSecond\nFirst", ExceptionHelper.getExceptionMessageWithCauses(e));
	}

	@Test
	public void shouldFormatException() {

		Exception e = nestedException();
		String formatException = ExceptionHelper.formatException(e);
		assertTrue(formatException.contains("java.lang.RuntimeException: First"));
		assertTrue(formatException.contains("java.lang.RuntimeException: Second"));
		assertTrue(formatException.contains("java.lang.RuntimeException: Third"));

		formatException = ExceptionHelper.formatExceptionMessage(e);
		assertEquals("Third\ncause:\nSecond\ncause:\nFirst", formatException);
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
