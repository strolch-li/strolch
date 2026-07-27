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

package li.strolch.policy;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import static li.strolch.utils.ClassScanningHelper.shouldIgnoreClassFile;
import static li.strolch.utils.ClassScanningHelper.shouldIgnoreFile;
import static li.strolch.utils.helper.ExceptionHelper.getRootCauseMessage;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchPolicyScanner {

	private static final Logger logger = LoggerFactory.getLogger(StrolchPolicyScanner.class);
	private static List<Class<? extends StrolchPolicy>> cachedPolicies;

	public static synchronized List<Class<? extends StrolchPolicy>> scan() {
		if (cachedPolicies != null)
			return cachedPolicies;

		long start = System.currentTimeMillis();
		Set<Class<? extends StrolchPolicy>> policies = new HashSet<>();
		try {
			ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
			Enumeration<URL> resources = classLoader.getResources("");
			List<URL> resourceList = Collections.list(resources);

			Set<File> classpathFiles = new HashSet<>();
			for (URL url : resourceList) {
				if (url.getProtocol().equals("file")) {
					classpathFiles.add(new File(url.getPath()));
				} else if (url.getProtocol().equals("jar")) {
					String path = url.getPath();
					if (path.startsWith("file:")) {
						path = path.substring(5);
					}
					int bangIndex = path.indexOf('!');
					if (bangIndex != -1) {
						path = path.substring(0, bangIndex);
					}
					classpathFiles.add(new File(path));
				} else {
					logger.warn("Ignoring URL: {}", url);
				}
			}

			// Also scan JARs
			String classPath = System.getProperty("java.class.path");
			String[] classPathElements = classPath.split(File.pathSeparator);
			for (String element : classPathElements) {
				classpathFiles.add(new File(element));
			}

			for (File file : classpathFiles) {
				if (shouldIgnoreFile(file))
					continue;

				if (file.isDirectory()) {
					scanDirectory(file, "", policies, classLoader);
				} else if (file.getName().endsWith(".jar")) {
					scanJar(file, policies, classLoader);
				}
			}

		} catch (IOException e) {
			logger.error("Failed to scan classpath for policies", e);
		}

		cachedPolicies = new ArrayList<>(policies);
		long end = System.currentTimeMillis();
		logger.info("Scanned {} StrolchPolicies in {}ms", cachedPolicies.size(), (end - start));
		return cachedPolicies;
	}

	private static void scanDirectory(File directory, String packageName, Set<Class<? extends StrolchPolicy>> policies,
			ClassLoader classLoader) {
		File[] files = directory.listFiles();
		if (files == null)
			return;

		for (File file : files) {
			if (shouldIgnoreFile(file))
				continue;

			if (file.isDirectory()) {
				scanDirectory(file, packageName + file.getName() + ".", policies, classLoader);
			} else {
				String name = file.getName();
				if (!name.endsWith(".class") || shouldIgnoreClassFile(name))
					continue;

				String className = packageName + name.substring(0, name.length() - 6);
				checkAndAdd(name, className, policies, classLoader);
			}
		}
	}

	private static void scanJar(File jarFile, Set<Class<? extends StrolchPolicy>> policies, ClassLoader classLoader) {
		String jarFileName = jarFile.getName();
		boolean classesFound = false;
		try (ZipInputStream zip = new ZipInputStream(jarFile.toURI().toURL().openStream())) {
			for (ZipEntry entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
				if (!entry.isDirectory() && entry.getName().endsWith(".class")) {
					if (shouldIgnoreClassFile(entry.getName()))
						continue;

					String className = entry.getName().replace('/', '.');
					className = className.substring(0, className.length() - 6);
					if (checkAndAdd(jarFileName, className, policies, classLoader))
						classesFound = true;
				}
			}
		} catch (IOException e) {
			logger.error("Failed to scan JAR {} for policies", jarFile.getAbsolutePath(), e);
		}
		if (!classesFound)
			logger.info("No classes found in JAR {}", jarFileName);
	}

	@SuppressWarnings("unchecked")
	private static boolean checkAndAdd(String sourceName, String className,
			Set<Class<? extends StrolchPolicy>> policies, ClassLoader classLoader) {
		try {
			Class<?> clazz = Class.forName(className, false, classLoader);
			if (StrolchPolicy.class.isAssignableFrom(clazz)
					&& !Modifier.isAbstract(clazz.getModifiers())
					&& !clazz.isInterface()) {
				policies.add((Class<? extends StrolchPolicy>) clazz);
				return true;
			}
		} catch (Throwable e) {
			logger.error("Failed to load policy class {} from {}: {}", className, sourceName, getRootCauseMessage(e));
		}

		return false;
	}
}
