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
import java.util.concurrent.atomic.AtomicBoolean;
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
	private static final MapOfMaps<String, Locale, ResourceBundle> bundleMap = new MapOfMaps<>();
	private static final AtomicBoolean loaded = new AtomicBoolean(false);
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
		this.key = key.intern();
		this.values = new Properties();
		this.bundle = bundle;
		this.bundleName = bundle.getBaseBundleName().intern();
	}

	public I18nMessage(String bundle, String key, Properties values, String message) {
		DBC.INTERIM.assertNotNull("bundle must not be empty!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		DBC.INTERIM.assertNotEmpty("message must be set!", message);
		this.key = key.intern();
		this.values = values == null ? new Properties() : values;
		this.message = message;
		this.bundle = findBundle(bundle);
		this.bundleName = this.bundle == null ? bundle : this.bundle.getBaseBundleName();
	}

	public I18nMessage(I18nMessage other) {
		this.key = other.key;
		this.values = new Properties(other.values);
		this.bundle = other.bundle;
		this.bundleName = other.bundleName;
		this.message = other.message;
		this.exception = other.exception;
		this.stackTrace = other.stackTrace;
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
		return this.values.getOrDefault(key, null);
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
				logger.error("Failed to find resource bundle {} {}, returning current bundle {}", baseName,
						locale.toLanguageTag(), this.bundle.getLocale().toLanguageTag());
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
				logger.warn("No bundle found for {} {}. Available are: ", this.bundleName, locale);
				getBundleMap().forEach((s, map) -> {
					logger.info("  {}", s);
					map.forEach((l, resourceBundle) -> logger.info("  {}: {}", l, map.keySet()));
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
				logger.error("Key {} is missing in bundle {} for locale {}", this.key, baseName, languageTag);
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
			return other.values == null;
		} else
			return this.values.equals(other.values);
	}

	@Override
	public String toString() {
		return getMessage(Locale.getDefault());
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
		synchronized (loaded) {
			if (!loaded.get())
				findAllBundles();
			return bundleMap;
		}
	}

	private static void findAllBundles() {
		try {
			CodeSource src = I18nMessage.class.getProtectionDomain().getCodeSource();
			if (src == null) {
				logger.error("Failed to find CodeSource for ProtectionDomain {}",
						I18nMessage.class.getProtectionDomain());
				return;
			}

			File jarLocationF = new File(src.getLocation().toURI());
			if (!(jarLocationF.exists() && jarLocationF.getParentFile().isDirectory())) {
				logger.info("Found JAR repository at {}", jarLocationF.getParentFile());
				return;
			}

			File jarD = jarLocationF.getParentFile();
			File[] jarFiles = jarD.listFiles((dir, name) -> name.endsWith(".jar"));
			if (jarFiles == null)
				return;

			for (File file : jarFiles) {

				if (shouldIgnoreFile(file))
					continue;

				try (JarFile jarFile = new JarFile(file)) {
					Enumeration<JarEntry> entries = jarFile.entries();
					while (entries.hasMoreElements()) {
						JarEntry je = entries.nextElement();

						String entryName = je.getName();
						if (!entryName.endsWith(".properties"))
							continue;

						if (shouldIgnorePropertyFile(entryName))
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
						logger.info("    Loaded bundle {} {} from {} from JAR {}", bundle.getBaseBundleName(),
								bundle.getLocale(), propertyName, file.getName());
					}
				}
			}

			File classesD = new File(jarD.getParentFile(), "classes");
			if (classesD.isDirectory()) {
				File[] propertyFiles = classesD.listFiles((dir, name) -> name.endsWith(".properties") && !(
						name.equals("appVersion.properties") || name.equals("ENV.properties")));
				if (propertyFiles != null) {
					for (File propertyFile : propertyFiles) {

						logger.info("  Found property file {} in classes {}", propertyFile.getName(),
								classesD.getAbsolutePath());

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
						logger.info("    Loaded bundle {} {} from file {}", bundle.getBaseBundleName(),
								bundle.getLocale(), propertyFile.getName());
					}
				}
			}

			logger.info("Done.");

		} catch (Exception e) {
			logger.error("Failed to find all property files!", e);
		} finally {
			loaded.set(true);
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
					locale = Locale.of(language, country);
				else {
					logger.warn("Ignoring malformed bad bundle locale for {}", entryName);
					return null;
				}
			} else {
				int languageI = Arrays.binarySearch(Locale.getISOLanguages(), localeS);
				if (languageI >= 0)
					locale = Locale.forLanguageTag(localeS);
				else {
					logger.warn("Ignoring bad bundle locale for {}", entryName);
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

	private static boolean shouldIgnorePropertyFile(String name) {
		return name.startsWith("META-INF")
				|| name.equals("ENV.properties")
				|| name.equals("agentVersion.properties")
				|| name.equals("appVersion.properties")
				|| name.equals("componentVersion.properties")
				|| name.contains("_db_version");
	}

	private static boolean shouldIgnoreFile(File file) {
		String name = file.getName();
		return name.contains("aopalliance")
				|| name.contains("activation")
				|| name.contains("antlr")
				|| name.contains("assertj-core")
				|| name.startsWith("com.sun")
				|| name.startsWith("commonj.")
				|| name.startsWith("commons-")
				|| name.startsWith("jackson-")
				|| name.startsWith("hapi-")
				|| name.startsWith("jaxb-")
				|| name.startsWith("org.hl7.")
				|| name.startsWith("org.glassfish.")
				|| name.startsWith("listenablefuture-")
				|| name.startsWith("j2objc-annotations")
				|| name.startsWith("failureaccess-")
				|| name.startsWith("error_prone_")
				|| name.startsWith("guava-")
				|| name.startsWith("org.eclipse")
				|| name.startsWith("javax")
				|| name.startsWith("jaxws")
				|| name.startsWith("jaxrs")
				|| name.startsWith("jaxb")
				// bouncy castle
				|| name.contains("-jdk18on-")
				|| name.contains("jsr305")
				|| name.contains("c3p0")
				|| name.contains("camel")
				|| name.contains("checker-qual")
				|| name.contains("cron")
				|| name.contains("FastInfoset")
				|| name.contains("gmbal")
				|| name.contains("grizzly")
				|| name.contains("gson")
				|| name.contains("ha-api")
				|| name.contains("HikariCP")
				|| name.contains("hk2")
				|| name.contains("icu4j")
				|| name.contains("jakarta")
				|| name.contains("javassist")
				|| name.contains("jersey")
				|| name.contains("joda-time")
				|| name.contains("logback")
				|| name.contains("management-api")
				|| name.contains("mchange-commons-java")
				|| name.contains("mimepull")
				|| name.contains("org.abego.treelayout")
				|| name.contains("osgi")
				|| name.contains("pfl-basic")
				|| name.contains("pfl-tf")
				|| name.contains("policy-2.7.10")
				|| name.contains("postgresql")
				|| name.contains("quartz")
				|| name.contains("saaj-impl")
				|| name.contains("sax")
				|| name.contains("slf4j")
				|| name.contains("ST4")
				|| name.contains("stax-ex")
				|| name.contains("stax2-api")
				|| name.contains("streambuffer")
				|| name.contains("tyrus")
				|| name.contains("validation-api")
				|| name.contains("yasson");
	}
}
