package li.strolch.utils.helper;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Locale;
import java.util.Properties;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.junit.Assume;
import org.junit.Test;

public class TexHelperTest {

	@Test
	public void test() {
		Assume.assumeThat(true, new PdfLatexChecker());

		File texDir = new File("src/test/resources");
		String templateName = "tex.tpl";
		String bundleName = "UtilsTest";
		Locale locale = new Locale("de", "CH");
		TexHelper helper = new TexHelper(texDir, templateName, bundleName, locale);

		String lastModified = "2018-01-21T04:01:00.000+02:00";
		String validFrom = "2018-01-21T04:01:00.000+02:00";
		String validTo = "2018-01-21T04:01:00.000+02:00";
		String validFor = "P1D";

		Properties properties = new Properties();
		properties.setProperty("doc_author", "Robert von Burg");
		properties.setProperty("doc_title", "Some test");
		properties.setProperty("obj_name", "Object Name");
		properties.setProperty("obj_id", "123");
		properties.setProperty("obj_last_modified", DateHelper.formatDate(locale, lastModified, false));
		properties.setProperty("validity_from", DateHelper.formatDate(locale, validFrom, true));
		properties.setProperty("validity_to", DateHelper.formatDate(locale, validTo, true));
		properties.setProperty("valid_for", DateHelper.formatPeriod(helper.getBundle(), null, validFor));

		String fileName = TexHelperTest.class.getSimpleName();
		File renderFile = helper.renderFile(properties, fileName);
		assertNotNull(renderFile);
		assertTrue(renderFile.exists());
	}

	private static class PdfLatexChecker extends TypeSafeMatcher<Boolean> {
		@Override
		protected boolean matchesSafely(Boolean b) {

			int result;
			try {
				Process exec = Runtime.getRuntime().exec("pdflatex --version");
				result = exec.waitFor();
			} catch (Exception e) {
				result = 99;
			}

			return b ? result == 0 : result > 1;
		}

		@Override
		public void describeTo(Description description) {
			description.appendText("validate PDF Latex is installed");
		}
	}
}
