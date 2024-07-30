package li.strolch.utils.helper;

import java.io.File;

/**
 * The options to create the path for a temporary file using {@link FileHelper#getTempFile(File, String, String, TempFileOptions)}
 */
public enum TempFileOptions {
	NONE,
	SEPARATE_DATE_SEGMENTS,
	WITH_HOURS,
	SEPARATE_HOURS,
	APPEND_MILLIS,
	MILLIS_FIRST
}
