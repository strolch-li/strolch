package li.strolch.utils;

import static java.util.Collections.emptySet;
import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfSets;
import static li.strolch.utils.helper.ExceptionHelper.formatException;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessageWithCauses;
import static li.strolch.utils.helper.StringHelper.EMPTY;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.CodeSource;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import li.strolch.utils.collections.MapOfMaps;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.collections.TypedTuple;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class I18nMessage {

	private static final Logger logger = LoggerFactory.getLogger(I18nMessage.class);
	private static MapOfMaps<String, Locale, ResourceBundle> bundleMap;
	private static final MapOfSets<String, String> missingKeysMap = synchronizedMapOfSets(new MapOfSets<>());

	private final String bundleName;
	private final String key;
	private final Properties values;
	private final ResourceBundle bundle;
	private String message;
	protected Throwable exception;
	protected String stackTrace;

	public I18nMessage(ResourceBundle bundle, String key) {
		DBC.INTERIM.assertNotNull("bundle may not be null!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.key = key;
		this.values = new Properties();
		this.bundle = bundle;
		this.bundleName = bundle.getBaseBundleName();
	}

	public I18nMessage(String bundle, String key, Properties values, String message) {
		DBC.INTERIM.assertNotNull("bundle must not be empty!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		DBC.INTERIM.assertNotEmpty("message must be set!", message);
		this.key = key;
		this.values = values == null ? new Properties() : values;
		this.message = message;
		this.bundle = findBundle(bundle);
		this.bundleName = this.bundle == null ? bundle : this.bundle.getBaseBundleName();
	}

	public I18nMessage(I18nMessage other) {
		this.key = other.key;
		this.values = other.values;
		this.bundle = other.bundle;
		this.bundleName = other.bundleName;
		this.message = other.message;
	}

	public String getKey() {
		return this.key;
	}

	public String getBundle() {
		if (this.bundle == null)
			return "";
		return this.bundle.getBaseBundleName();
	}

	public Properties getValues() {
		return this.values;
	}

	public Object getValue(String key) {
		return this.values.get(key);
	}

	private ResourceBundle getBundle(Locale locale) {
		if (this.bundle == null)
			return null;
		if (this.bundle.getLocale() == locale)
			return this.bundle;
		String baseName = this.bundle.getBaseBundleName();

		try {

			ClassLoader classLoader = this.bundle.getClass().getClassLoader();
			if (classLoader == null)
				return ResourceBundle.getBundle(baseName, locale);
			return ResourceBundle.getBundle(baseName, locale, classLoader);

		} catch (MissingResourceException e) {
			if (!missingKeysMap.containsSet(baseName + "_" + locale.toLanguageTag())) {
				logger.error("Failed to find resource bundle " + baseName + " " + locale.toLanguageTag()
						+ ", returning current bundle " + this.bundle.getLocale().toLanguageTag());
				missingKeysMap.addSet(baseName + "_" + locale.toLanguageTag(), emptySet());
			}
			return this.bundle;
		}
	}

	public String getMessage(ResourceBundle bundle) {
		DBC.INTERIM.assertNotNull("bundle may not be null!", bundle);
		return formatMessage(bundle);
	}

	public String getMessage(Locale locale) {
		ResourceBundle bundle = getBundle(locale);
		if (bundle == null) {
			if (isEmpty(this.bundleName))
				return getMessage();
			if (!missingKeysMap.containsSet(this.bundleName + "_" + locale.toLanguageTag())) {
				logger.warn("No bundle found for " + this.bundleName + " " + locale + ". Available are: ");
				getBundleMap().forEach((s, map) -> {
					logger.info("  " + s);
					map.forEach((l, resourceBundle) -> logger.info("  " + l + ": " + map.keySet()));
				});
				missingKeysMap.addSet(this.bundleName + "_" + locale.toLanguageTag(), emptySet());
			}
			return getMessage();
		}
		return formatMessage(bundle);
	}

	public String getMessage() {
		return formatMessage();
	}

	public I18nMessage value(String key, Object value) {
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.values.setProperty(key, value == null ? "(null)" : value.toString());
		return this;
	}

	public I18nMessage value(String key, Throwable t) {
		this.exception = t;
		this.stackTrace = formatException(t);
		value(key, getExceptionMessageWithCauses(t));
		return this;
	}

	public I18nMessage withException(Throwable t) {
		this.exception = t;
		this.stackTrace = formatException(t);
		return this;
	}

	public boolean hasException() {
		return this.exception != null;
	}

	public Throwable getException() {
		return exception;
	}

	public String getStackTrace() {
		return this.stackTrace;
	}

	public String formatMessage() {
		if (this.message != null)
			return this.message;

		if (this.bundle == null) {
			this.message = this.key;
			return this.message;
		}

		this.message = formatMessage(this.bundle);
		return this.message;
	}

	public String formatMessage(ResourceBundle bundle) {
		try {
			String string = bundle.getString(this.key);
			return StringHelper.replacePropertiesIn(this.values, EMPTY, string);
		} catch (MissingResourceException e) {
			String baseName = bundle.getBaseBundleName();
			String languageTag = bundle.getLocale().toLanguageTag();
			String bundleKey = baseName + "_" + languageTag;
			if (!missingKeysMap.containsElement(bundleKey, this.key)) {
				logger.error("Key " + this.key + " is missing in bundle " + baseName + " for locale " + languageTag);
				missingKeysMap.addElement(bundleKey, this.key);
			}

			return this.key;
		}
	}

	public <T> T accept(I18nMessageVisitor<T> visitor) {
		return visitor.visit(this);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.key == null) ? 0 : this.key.hashCode());
		result = prime * result + ((this.values == null) ? 0 : this.values.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		I18nMessage other = (I18nMessage) obj;
		if (this.key == null) {
			if (other.key != null)
				return false;
		} else if (!this.key.equals(other.key))
			return false;
		if (this.values == null) {
			if (other.values != null)
				return false;
		} else if (!this.values.equals(other.values))
			return false;
		return true;
	}

	private ResourceBundle findBundle(String baseName) {
		if (baseName.isEmpty())
			return null;

		Map<Locale, ResourceBundle> bundlesByLocale = getBundleMap().getMap(baseName);
		if (bundlesByLocale == null || bundlesByLocale.isEmpty())
			return null;

		ResourceBundle bundle = bundlesByLocale.get(Locale.getDefault());
		if (bundle != null)
			return bundle;

		return bundlesByLocale.values().iterator().next();
	}

	private static MapOfMaps<String, Locale, ResourceBundle> getBundleMap() {
		if (bundleMap == null) {
			synchronized (I18nMessage.class) {
				bundleMap = findAllBundles();
			}
		}

		return bundleMap;
	}

	private static MapOfMaps<String, Locale, ResourceBundle> findAllBundles() {
		try {
			CodeSource src = I18nMessage.class.getProtectionDomain().getCodeSource();
			if (src == null) {
				logger.error(
						"Failed to find CodeSource for ProtectionDomain " + I18nMessage.class.getProtectionDomain());
				return new MapOfMaps<>();
			}

			File jarLocationF = new File(src.getLocation().toURI());
			if (!(jarLocationF.exists() && jarLocationF.getParentFile().isDirectory())) {
				logger.info("Found JAR repository at " + jarLocationF.getParentFile());
				return new MapOfMaps<>();
			}

			MapOfMaps<String, Locale, ResourceBundle> bundleMap = new MapOfMaps<>();

			File jarD = jarLocationF.getParentFile();
			File[] jarFiles = jarD.listFiles((dir, name) -> name.endsWith(".jar"));
			if (jarFiles == null)
				return new MapOfMaps<>();

			for (File file : jarFiles) {

				if (shouldIgnoreFile(file))
					continue;

				try (JarFile jarFile = new JarFile(file)) {
					Enumeration<JarEntry> entries = jarFile.entries();
					while (entries.hasMoreElements()) {
						JarEntry je = entries.nextElement();

						String entryName = je.getName();

						if (entryName.startsWith("META-INF") //
								|| entryName.equals("ENV.properties") //
								|| entryName.equals("agentVersion.properties") //
								|| entryName.equals("appVersion.properties") //
								|| entryName.equals("componentVersion.properties") //
								|| entryName.equals("strolch_db_version.properties"))
							continue;

						if (!entryName.endsWith(".properties"))
							continue;

						TypedTuple<String, Locale> tuple = parsePropertyName(entryName);
						if (tuple == null)
							continue;
						String baseName = tuple.getFirst();
						Locale locale = tuple.getSecond();

						ResourceBundle bundle = ResourceBundle.getBundle(baseName, locale,
								new CustomControl(jarFile.getInputStream(je)));

						bundleMap.addElement(bundle.getBaseBundleName(), bundle.getLocale(), bundle);

						String propertyName = entryName.replace('/', '.');
						logger.info(
								"    Loaded bundle " + bundle.getBaseBundleName() + " " + bundle.getLocale() + " from "
										+ propertyName + " from JAR " + file.getName());
					}
				}
			}

			File classesD = new File(jarD.getParentFile(), "classes");
			if (classesD.isDirectory()) {
				File[] propertyFiles = classesD.listFiles(
						(dir, name) -> name.endsWith(".properties") && !(name.equals("appVersion.properties")
								|| name.equals("ENV.properties")));
				if (propertyFiles != null && propertyFiles.length > 0) {
					for (File propertyFile : propertyFiles) {

						logger.info("  Found property file " + propertyFile.getName() + " in classes "
								+ classesD.getAbsolutePath());

						TypedTuple<String, Locale> tuple = parsePropertyName(propertyFile.getName());
						if (tuple == null)
							continue;
						String baseName = tuple.getFirst();
						Locale locale = tuple.getSecond();

						ResourceBundle bundle;
						try (FileInputStream in = new FileInputStream(propertyFile)) {
							bundle = ResourceBundle.getBundle(baseName, locale, new CustomControl(in));
						}

						bundleMap.addElement(bundle.getBaseBundleName(), bundle.getLocale(), bundle);
						logger.info("    Loaded bundle " + bundle.getBaseBundleName() + " " + bundle.getLocale()
								+ " from file " + propertyFile.getName());
					}
				}
			}

			logger.info("Done.");
			return bundleMap;

		} catch (Exception e) {
			logger.error("Failed to find all property files!", e);
			return new MapOfMaps<>();
		}
	}

	private static TypedTuple<String, Locale> parsePropertyName(String entryName) {
		String propertyName = entryName.replace('/', '.');

		String bundleName = propertyName.substring(0, propertyName.lastIndexOf("."));
		String baseName;
		Locale locale;
		int i = bundleName.indexOf('_');
		if (i > 0) {
			baseName = bundleName.substring(0, i);
			String localeS = bundleName.substring(i + 1);
			String[] parts = localeS.split("_");
			if (parts.length == 2) {
				String language = parts[0];
				String country = parts[1];
				int languageI = Arrays.binarySearch(Locale.getISOLanguages(), language);
				int countryI = Arrays.binarySearch(Locale.getISOCountries(), country);
				if (languageI >= 0 && countryI >= 0)
					locale = new Locale(language, country);
				else {
					logger.warn("Ignoring bad bundle locale for " + entryName);
					return null;
				}
			} else {
				int languageI = Arrays.binarySearch(Locale.getISOLanguages(), localeS);
				if (languageI >= 0)
					locale = new Locale(localeS);
				else {
					logger.warn("Ignoring bad bundle locale for " + entryName);
					return null;
				}
			}
		} else {
			baseName = bundleName;
			locale = Locale.getDefault();
		}

		return new TypedTuple<>(baseName, locale);
	}

	private static class CustomControl extends ResourceBundle.Control {
		private final InputStream stream;

		public CustomControl(InputStream stream) {
			this.stream = stream;
		}

		@Override
		public ResourceBundle newBundle(String baseName, Locale locale, String format, ClassLoader loader,
				boolean reload) throws IOException {
			return new PropertyResourceBundle(this.stream);
		}
	}

	private static boolean shouldIgnoreFile(File file) {
		return file.getName().contains("aopalliance") //
				|| file.getName().contains("activation") //
				|| file.getName().contains("antlr") //
				|| file.getName().contains("assertj-core") //
				|| file.getName().startsWith("com.sun") //
				|| file.getName().startsWith("commonj.") //
				|| file.getName().startsWith("commons-") //
				|| file.getName().startsWith("jackson-") //
				|| file.getName().startsWith("hapi-") //
				|| file.getName().startsWith("org.hl7.") //
				|| file.getName().startsWith("listenablefuture-") //
				|| file.getName().startsWith("j2objc-annotations") //
				|| file.getName().startsWith("failureaccess-") //
				|| file.getName().startsWith("error_prone_") //
				|| file.getName().startsWith("guava-") //
				|| file.getName().startsWith("org.eclipse") //
				|| file.getName().contains("jsr305") //
				|| file.getName().contains("c3p0") //
				|| file.getName().contains("camel") //
				|| file.getName().contains("checker-qual") //
				|| file.getName().contains("cron") //
				|| file.getName().contains("FastInfoset") //
				|| file.getName().contains("gmbal") //
				|| file.getName().contains("grizzly") //
				|| file.getName().contains("gson") //
				|| file.getName().contains("ha-api") //
				|| file.getName().contains("HikariCP") //
				|| file.getName().contains("hk2") //
				|| file.getName().contains("icu4j") //
				|| file.getName().contains("jakarta") //
				|| file.getName().contains("javassist") //
				|| file.getName().contains("javax") //
				|| file.getName().contains("jaxb-api") //
				|| file.getName().contains("jaxb-core") //
				|| file.getName().contains("jaxb-impl") //
				|| file.getName().contains("jaxrs-ri") //
				|| file.getName().contains("jaxws-rt") //
				|| file.getName().contains("jaxws-rt") //
				|| file.getName().contains("jersey") //
				|| file.getName().contains("joda-time") //
				|| file.getName().contains("logback") //
				|| file.getName().contains("management-api") //
				|| file.getName().contains("mchange-commons-java") //
				|| file.getName().contains("mimepull") //
				|| file.getName().contains("org.abego.treelayout") //
				|| file.getName().contains("osgi") //
				|| file.getName().contains("pfl-basic") //
				|| file.getName().contains("pfl-tf") //
				|| file.getName().contains("policy-2.7.10") //
				|| file.getName().contains("postgresql") //
				|| file.getName().contains("quartz") //
				|| file.getName().contains("saaj-impl") //
				|| file.getName().contains("sax") //
				|| file.getName().contains("slf4j") //
				|| file.getName().contains("ST4") //
				|| file.getName().contains("stax-ex") //
				|| file.getName().contains("stax2-api") //
				|| file.getName().contains("streambuffer") //
				|| file.getName().contains("tyrus") //
				|| file.getName().contains("validation-api") //
				|| file.getName().contains("yasson");
	}
}
