package li.strolch.utils.helper;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.utils.UTF8Control;

/**
 * Created by eitch on 03.11.16.
 */
public class TexHelper {

	private static final Logger logger = LoggerFactory.getLogger(TexHelper.class);

	private final File texDir;
	private final File tempDir;
	private final String templateName;
	private final ResourceBundle bundle;

	public TexHelper(File texDir, String templateName, String bundleName, Locale locale) {
		this(texDir, new File(System.getProperty("java.io.tmpdir")), templateName, bundleName, locale);
	}

	public TexHelper(File texDir, File tempDir, String templateName, String bundleName, Locale locale) {
		this.texDir = texDir;
		this.tempDir = tempDir;
		this.templateName = templateName;
		this.bundle = ResourceBundle.getBundle(bundleName, locale, new UTF8Control());
	}

	public ResourceBundle getBundle() {
		return this.bundle;
	}

	public File renderFile(Properties properties, String fileName) {

		String data;
		File templatePath = new File(this.texDir, this.templateName);
		try {
			data = new String(Files.readAllBytes(templatePath.toPath()));
		} catch (IOException e) {
			throw new RuntimeException("Failed to read template from " + templatePath.getAbsolutePath());
		}

		data = StringHelper.replacePropertiesIn(properties, data);
		data = StringHelper.replacePropertiesIn(getI18nData(bundle), "%", data);

		File renderedPdf = renderPdf(fileName, templatePath, data);

		return renderedPdf;
	}

	private File renderPdf(String fileName, File templatePath, String data) {
		try {
			Path tempPath = this.tempDir.toPath();
			Path texPath = tempPath.resolve(TexHelper.class.getName());
			if (!texPath.toFile().exists()) {
				if (!texPath.toFile().mkdir()) {
					throw new RuntimeException("Failed to create temp path " + texPath);
				}
			}

			// clean old builds
			Arrays.asList(texPath.toFile().listFiles(
					file -> (System.currentTimeMillis() - file.lastModified()) > TimeUnit.MINUTES.toMillis(1)))
					.forEach(file -> {
						logger.info("Deleting old path " + file);
						FileHelper.deleteFile(file, false);
					});

			// prepare a temporary directory by copying tex files
			File tmpPathF = texPath.resolve(StringHelper.getUniqueId()).toFile();
			if (!tmpPathF.mkdir()) {
				throw new RuntimeException("Failed to create temp path " + tmpPathF);
			}

			if (!FileHelper.copy(templatePath.getParentFile().listFiles(), tmpPathF, false))
				throw new RuntimeException("Failed to copy " + templatePath.getParentFile().getAbsolutePath()
						+ " to tmpPath " + tmpPathF.getAbsolutePath());

			// then write TEX file
			String texFileName = fileName + ".tex";
			File texFileS = new File(tmpPathF, texFileName);
			try (OutputStreamWriter out = new OutputStreamWriter(Files.newOutputStream(texFileS.toPath()), "UTF-8")) {
				out.write(data);
			}

			// do PDF generation
			String cmd = "pdflatex";
			ProcessHelper.ProcessResult processResult = ProcessHelper.runCommand(tmpPathF, cmd, "-halt-on-error",
					"-file-line-error", texFileName);
			if (processResult.returnValue != 0) {
				logger.error(processResult.processOutput);
				throw new RuntimeException(
						"Failed to do PDF generation due to error return value from pdflatex command, see logs for result.");
			}

			File pdfResultF = new File(tmpPathF, fileName + ".pdf");
			if (!pdfResultF.isFile()) {
				throw new RuntimeException("Expected generated PDF does not exist!");
			}

			return pdfResultF;

		} catch (IOException e) {
			throw new RuntimeException("Failed to render PDF", e);
		}
	}

	private Properties getI18nData(ResourceBundle bundle) {
		Properties i18nProps = new Properties();
		Enumeration<String> keys = bundle.getKeys();
		while (keys.hasMoreElements()) {
			String key = keys.nextElement();
			i18nProps.setProperty(key, bundle.getString(key));
		}

		return i18nProps;
	}
}
