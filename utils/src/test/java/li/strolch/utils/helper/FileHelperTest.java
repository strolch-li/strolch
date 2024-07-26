package li.strolch.utils.helper;

import static li.strolch.utils.helper.FileHelper.getTempFile;
import static li.strolch.utils.helper.StringHelper.normalizeLength;
import static li.strolch.utils.helper.TempFileOptions.*;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.text.MessageFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Set;

import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FileHelperTest {
	private static final Logger logger = LoggerFactory.getLogger(FileHelperTest.class);

	private static File tempPath;

	@BeforeClass
	public static void beforeClass() {
		tempPath = new File(System.getProperty("java.io.tmpdir"));
	}

	@Test
	public void shouldCreateTempFileNoOptions() {
		File path = getTempFile(tempPath, "NoOptions", ".txt");
		logger.info("Temp path: {}", path.getAbsolutePath());

		LocalDate today = LocalDate.now();
		String expected = today + "/NoOptions.txt";
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments() {
		Set<TempFileOptions> options = Set.of(SEPARATE_DATE_SEGMENTS);
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt", options);
		logger.info("Temp path: {}", path.getAbsolutePath());

		LocalDate today = LocalDate.now();
		String month = normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0');
		String day = normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0');
		String expected = MessageFormat.format("{0,number,#}/{1}/{2}/SeparateDateSegments.txt", today.getYear(), month,
				day);
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments_WithHours() {
		Set<TempFileOptions> options = Set.of(SEPARATE_DATE_SEGMENTS, WITH_HOURS);
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt", options);
		logger.info("Temp path: {}", path.getAbsolutePath());

		LocalDateTime today = LocalDateTime.now();
		String month = normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0');
		String day = normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0');
		String hour = normalizeLength(String.valueOf(today.getHour()), 2, true, '0');
		String expected = MessageFormat.format("{0,number,#}/{1}/{2}_{3}/SeparateDateSegments.txt", today.getYear(),
				month, day, hour);
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments_WithHours_SeparateHours() {
		Set<TempFileOptions> options = Set.of(SEPARATE_DATE_SEGMENTS, WITH_HOURS, SEPARATE_HOURS);
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt", options);
		logger.info("Temp path: {}", path.getAbsolutePath());

		LocalDateTime today = LocalDateTime.now();
		String month = normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0');
		String day = normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0');
		String hour = normalizeLength(String.valueOf(today.getHour()), 2, true, '0');
		String expected = MessageFormat.format("{0,number,#}/{1}/{2}/{3}/SeparateDateSegments.txt", today.getYear(),
				month, day, hour);
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments_WithHours_SeparateHoursWithMillis() {
		LocalDateTime today = LocalDateTime.now();
		long timestamp = System.currentTimeMillis();

		Set<TempFileOptions> options = Set.of(SEPARATE_DATE_SEGMENTS, WITH_HOURS, SEPARATE_HOURS, APPEND_MILLIS);
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt", options, today, timestamp);
		logger.info("Temp path: {}", path.getAbsolutePath());

		String month = normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0');
		String day = normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0');
		String hour = normalizeLength(String.valueOf(today.getHour()), 2, true, '0');
		int year = today.getYear();
		String expected = MessageFormat.format("{0,number,#}/{1}/{2}/{3}/SeparateDateSegments_{4,number,#}.txt", year,
				month, day, hour, timestamp);
		assertEquals(new File(tempPath, expected), path);
	}

	@Test
	public void shouldCreateTempFileSeparateDateSegments_WithHours_SeparateHoursWithMillisFirst() {
		LocalDateTime today = LocalDateTime.now();
		long timestamp = System.currentTimeMillis();

		Set<TempFileOptions> options = Set.of(SEPARATE_DATE_SEGMENTS, WITH_HOURS, SEPARATE_HOURS, APPEND_MILLIS,
				MILLIS_FIRST);
		File path = getTempFile(tempPath, "SeparateDateSegments", ".txt", options, today, timestamp);
		logger.info("Temp path: {}", path.getAbsolutePath());

		String month = normalizeLength(String.valueOf(today.getMonthValue()), 2, true, '0');
		String day = normalizeLength(String.valueOf(today.getDayOfMonth()), 2, true, '0');
		String hour = normalizeLength(String.valueOf(today.getHour()), 2, true, '0');
		int year = today.getYear();
		String expected = MessageFormat.format("{0,number,#}/{1}/{2}/{3}/{4,number,#}_SeparateDateSegments.txt", year,
				month, day, hour, timestamp);
		assertEquals(new File(tempPath, expected), path);
	}
}
