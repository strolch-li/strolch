package li.strolch.utils.helper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ExceptionHelperTest {

	@Test
	public void shouldGetExceptionMsg() {

		Exception e = nestedException();
		assertEquals("java.lang.RuntimeException: Third", ExceptionHelper.getExceptionMessage(e));
		assertEquals("""
				java.lang.RuntimeException: Third
				java.lang.RuntimeException: Second
				java.lang.RuntimeException: First""", ExceptionHelper.getExceptionMessageWithCauses(e));
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
				java.lang.RuntimeException: Third
				cause: java.lang.RuntimeException: Second
				cause: java.lang.RuntimeException: First""", formatException);
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
