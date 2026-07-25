/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package li.strolch.utils;

import com.google.gson.JsonObject;
import li.strolch.utils.collections.MapOfMaps;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.collections.TypedTuple;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.CodeSource;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import static java.util.Collections.emptySet;
import static li.strolch.utils.ClassScanningHelper.shouldIgnoreFile;
import static li.strolch.utils.ClassScanningHelper.shouldIgnorePropertyFile;
import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfSets;
import static li.strolch.utils.helper.ExceptionHelper.formatException;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessageWithCauses;
import static li.strolch.utils.helper.StringHelper.isEmpty;

public class I18nMessage {

	private static final Logger logger = LoggerFactory.getLogger(I18nMessage.class);
	private static final MapOfMaps<String, Locale, ResourceBundle> bundleMap = new MapOfMaps<>();
	private static final AtomicBoolean loaded = new AtomicBoolean(false);
	private static final MapOfSets<String, String> missingKeysMap = synchronizedMapOfSets(new MapOfSets<>());

	private final String bundleName;
	private final String key;
	private final Map<String, String> values;
	private final ResourceBundle bundle;
	private String message;
	protected Throwable exception;
	protected String stackTrace;

	public I18nMessage(ResourceBundle bundle, String key) {
		DBC.INTERIM.assertNotNull("bundle may not be null!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.key = key.intern();
		this.values = new HashMap<>();
		this.bundle = bundle;
		this.bundleName = bundle.getBaseBundleName().intern();
	}

	public I18nMessage(String bundle, String key, Map<String, String> values, String message) {
		DBC.INTERIM.assertNotNull("bundle must not be empty!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		DBC.INTERIM.assertNotEmpty("message must be set!", message);
		this.key = key.intern();
		this.values = values == null ? new HashMap<>() : values;
		this.message = message;
		this.bundle = findBundle(bundle);
		this.bundleName = this.bundle == null ? bundle : this.bundle.getBaseBundleName();
	}

	public I18nMessage(I18nMessage other) {
		this.key = other.key;
		this.values = new HashMap<>(other.values);
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

	public Map<String, String> getValues() {
		return this.values;
	}

	public JsonObject getValuesAsJson() {
		JsonObject valuesJ = new JsonObject();
		values.forEach(valuesJ::addProperty);
		return valuesJ;
	}

	public String getValue(String key) {
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
					map.forEach((l, _) -> logger.info("  {}: {}", l, map.keySet()));
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
		this.values.put(key, value == null ? "(null)" : value.toString());
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
			return StringHelper.format(string, this.values);
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
			File[] jarFiles = jarD.listFiles((_, name) -> name.endsWith(".jar"));
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

						String propertyName = entryName.replace('/', '.');
						ResourceBundle bundle;
						try {
							bundle = ResourceBundle.getBundle(baseName, locale,
									new CustomControl(jarFile.getInputStream(je)));
						} catch (Exception e) {
							logger.error("Failed to load bundle {} {} from {} from JAR {}", baseName, locale,
									propertyName, file.getName(), e);
							continue;
						}

						bundleMap.addElement(bundle.getBaseBundleName(), bundle.getLocale(), bundle);
						logger.info("    Loaded bundle {} {} from {} from JAR {}", bundle.getBaseBundleName(),
								bundle.getLocale(), propertyName, file.getName());
					}
				} catch (Exception e) {
					logger.error("Failed to read JAR {}", file.getName(), e);
				}
			}

			File classesD = new File(jarD.getParentFile(), "classes");
			if (classesD.isDirectory()) {
				File[] propertyFiles = classesD.listFiles((_, name) -> name.endsWith(".properties") && !(
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
						} catch (Exception e) {
							logger.error("Failed to load bundle {} {} from file {}", baseName, locale,
									propertyFile.getName(), e);
							continue;
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
}
