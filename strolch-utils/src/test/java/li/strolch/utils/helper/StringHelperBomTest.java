package li.strolch.utils.helper;

import org.junit.Test;
import java.nio.charset.StandardCharsets;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class StringHelperBomTest {

	@Test
	public void shouldHaveCorrectBomValue() {
		assertEquals("\uFEFF", StringHelper.UTF8_BOM);
	}

	@Test
	public void shouldHaveCorrectBomBytes() {
		byte[] expected = {(byte) 0xEF, (byte) 0xBB, (byte) 0xBF};
		assertArrayEquals(expected, StringHelper.UTF8_BOM.getBytes(StandardCharsets.UTF_8));
	}
}
