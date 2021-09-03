package li.strolch.utils.helper;

import static li.strolch.utils.helper.FileHelper.*;
import static li.strolch.utils.helper.StringHelper.*;
import static li.strolch.utils.helper.TempFileOptions.*;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Set;

import org.junit.BeforeClass;
import org.junit.Test;

public class FileHelperTest {

	private static File tempPath;

	@BeforeClass
	public static void beforeClass() {
		tempPath = new File(System.getProperty("java.io.tmpdir"));
	}

	@Test
	public void shouldCreateTempFileNoOptions() {
		File path = getTempFile(tempPath, "NoOptions", ".txt");
		LocalDate today = LocalDate.now();
		String expected = today + "/NoOptions.txt";
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments() {
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt", Set.of(SEPARATE_DATE_SEGMENTS));
		LocalDate today = LocalDate.now();
		String expected =
				today.getYear() + "/" + normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0') + "/"
						+ normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0')
						+ "/SeparateDateSegments.txt";
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments_WithHours() {
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt", Set.of(SEPARATE_DATE_SEGMENTS, WITH_HOURS));
		LocalDateTime today = LocalDateTime.now();
		String expected =
				today.getYear() + "/" + normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0') + "/"
						+ normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0') + "_" + normalizeLength(
						String.valueOf(today.getHour()), 2, true, '0') + "/SeparateDateSegments.txt";
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments_WithHours_SeparateHours() {
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt",
				Set.of(SEPARATE_DATE_SEGMENTS, WITH_HOURS, SEPARATE_HOURS));
		LocalDateTime today = LocalDateTime.now();
		String expected =
				today.getYear() + "/" + normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0') + "/"
						+ normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0') + "/" + normalizeLength(
						String.valueOf(today.getHour()), 2, true, '0') + "/SeparateDateSegments.txt";
		assertEquals(new File(tempPath, expected), path);
	}
}
