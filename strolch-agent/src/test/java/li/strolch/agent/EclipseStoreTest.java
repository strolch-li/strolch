/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent;

import li.strolch.model.Resource;
import li.strolch.utils.helper.SystemHelper;
import org.eclipse.serializer.collections.lazy.LazyHashMap;
import org.eclipse.serializer.reference.Lazy;
import org.eclipse.serializer.reference.Referencing;
import org.eclipse.store.afs.nio.types.NioFileSystem;
import org.eclipse.store.storage.embedded.types.EmbeddedStorageFoundation;
import org.eclipse.store.storage.embedded.types.EmbeddedStorageManager;
import org.eclipse.store.storage.types.*;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Path;
import java.security.SecureRandom;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static li.strolch.model.ModelGenerator.*;
import static li.strolch.utils.helper.StringHelper.generateId;
import static org.junit.Assert.assertEquals;

@Ignore("Test on how to use EclipseStore")
public class EclipseStoreTest {

	private static final Logger log = LoggerFactory.getLogger(EclipseStoreTest.class);
	public static final int DATA_SIZE = 3000;
	public static final String STORAGE_PATH = "target/" + EclipseStoreTest.class.getSimpleName();
	private static EmbeddedStorageManager storageManager;
	private static ResourceRoot root;

	public static final String CAR = "Car";
	public static final String BICYCLE = "Bicycle";
	public static final String MOTORBIKE = "Motorbike";
	public static final String E_BIKE = "E-Bike";
	public static final String TRIKE = "Trike";
	private static final String[] TYPES = new String[]{CAR, BICYCLE, MOTORBIKE, E_BIKE, TRIKE};

	@BeforeClass
	public static void beforeClass() {
		log.info("Starting storage manager...");
		long start = System.currentTimeMillis();
		storageManager = buildFoundation("TEST", false).start();
		root = (ResourceRoot) storageManager.root();
		log.info("Started storage manager in {}ms", System.currentTimeMillis() - start);
		log.info(SystemHelper.getMemorySummary());
	}

	@AfterClass
	public static void afterClass() {
		storageManager.shutdown();
	}

	private static EmbeddedStorageFoundation<?> buildFoundation(String databaseName, boolean enableBackup) {
		Path databasePath = new File(STORAGE_PATH, databaseName).toPath();
		NioFileSystem fileSystem = NioFileSystem.New();

		int channelCount = 1;
		StorageConfiguration.Builder<?> storageConfigurationBuilder = StorageConfiguration
				.Builder()
				.setStorageFileProvider(Storage
						.FileProviderBuilder(fileSystem)
						.setDirectory(fileSystem.ensureDirectory(databasePath))
						.createFileProvider())
				.setChannelCountProvider(StorageChannelCountProvider.New(channelCount));

		if (enableBackup) {
			Path backupPath = new File(STORAGE_PATH, databaseName + "_Backup").toPath();
			storageConfigurationBuilder.setBackupSetup(StorageBackupSetup.New(fileSystem.ensureDirectory(backupPath)));
		}

		return EmbeddedStorageFoundation
				.New()
				.setDataBaseName(databaseName)
				.setConfiguration(storageConfigurationBuilder.createConfiguration());
	}

	@Test
	public void test() {
		if (root == null) {
			log.info("Initializing...");
			long start = System.currentTimeMillis();
			root = new ResourceRoot();
			root.createdAt = ZonedDateTime.now();
			storageManager.setRoot(root);
			addResources(root);
			storageManager.storeRoot();
			log.info("Initialized in {}ms", System.currentTimeMillis() - start);
			log.info("Initializing at: {}", root.createdAt);
			log.info(SystemHelper.getMemorySummary());
		} else {
			log.info("Root created: {}", root.createdAt);
			testResources(root);
		}
	}

	private void testResources(ResourceRoot root) {
		log.info("Testing resources...");
		long start = System.currentTimeMillis();
		assertEquals(DATA_SIZE, root.size());
		log.info("root.size() found {} resources.", root.size());
		root.resourceByType.keySet().forEach(type -> {
			log.info("{}: {}", type, root.resourceByType.get(type));
		});
		log.info("Tested root.size() in {}ms", System.currentTimeMillis() - start);
		log.info(SystemHelper.getMemorySummary());
		start = System.currentTimeMillis();
		AtomicInteger counter = new AtomicInteger();
		root.stream().forEach(_ -> counter.incrementAndGet());
		assertEquals(DATA_SIZE, counter.get());
		log.info("root.stream().forEach found {} resources.", counter.get());
		log.info("Tested root.stream().forEach() in {}ms", System.currentTimeMillis() - start);
		log.info(SystemHelper.getMemorySummary());
		start = System.currentTimeMillis();
		root.stream().forEach(resource -> {
			if (counter.incrementAndGet() % 100 == 0)
				log.info("Found {} {}: {}", resource.getType(), resource.getId(),
						resource.getString(BAG_ID, PARAM_STRING_ID));
		});
		log.info("Tested root.stream().forEach(resource -> ...) in {}ms", System.currentTimeMillis() - start);

		String type = CAR;
		String id = "TEST";
		Resource resource = root.getResourceBy(type, id);
		log.info("Testing {} :\n{}", Resource.locatorFor(type, id), resource.toXmlString());
		log.info(SystemHelper.getMemorySummary());
	}

	private void addResources(ResourceRoot root) {
		log.info("There are {} known resources.", root.size());
		long start = System.currentTimeMillis();
		SecureRandom random = new SecureRandom();
		for (int i = 0; i < DATA_SIZE - 1; i++) {
			String type = TYPES[random.nextInt(TYPES.length)];
			Resource resource = createResource(generateId(12), type, type);
			root.addResource(storageManager, resource);
			if (i % 100 == 0) {
				log.info("Added {} {} | {}", i, resource, SystemHelper.getMemorySummary());
			} else {
				root.clear(type);
			}
		}

		Resource resource = createResource("TEST", CAR, CAR);
		root.addResource(storageManager, resource);

		log.info("Added {} resource types with {} elements which took {}ms", root.resourceByType.size(), root.size(),
				System.currentTimeMillis() - start);
	}

	public static class ResourceRoot {
		private ZonedDateTime createdAt;

		private final Map<String, LazyHashMap<String, Lazy<Resource>>> resourceByType = new HashMap<>();

		public void addResource(StorageManager storageManager, Resource resource) {
			LazyHashMap<String, Lazy<Resource>> resourcesById = this.resourceByType.computeIfAbsent(resource.getType(),
					_ -> new LazyHashMap<>());
			boolean newMap = resourcesById.isEmpty();
			Lazy<Resource> reference = Lazy.Reference(resource);
			resourcesById.put(resource.getId(), reference);
			if (newMap)
				storageManager.storeAll(resourceByType, resourcesById);
			else
				storageManager.store(resourcesById);
		}

		public void clear(String type) {
			LazyHashMap<String, Lazy<Resource>> map = this.resourceByType.get(type);
			if (map != null)
				map.values().forEach(resource -> resource.clear());
		}

		public Stream<Resource> stream() {
			return this.resourceByType.values().stream().flatMap(map -> map.values().stream().map(Referencing::get));
		}

		public int size() {
			return this.resourceByType.values().stream().mapToInt(Map::size).sum();
		}

		@Override
		public String toString() {
			return "ResourceRoot{" + "createdAt=" + createdAt + '}';
		}

		public Resource getResourceBy(String type, String id) {
			return this.resourceByType.get(type).get(id).get();
		}
	}
}
