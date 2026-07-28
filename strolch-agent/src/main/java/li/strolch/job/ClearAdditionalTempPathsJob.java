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

package li.strolch.job;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * A job to clear arbitrary temporary paths defined in the {@link RuntimeConfiguration}.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ClearAdditionalTempPathsJob extends StrolchJob {

	private static final Logger logger = LoggerFactory.getLogger(ClearAdditionalTempPathsJob.class);

	public ClearAdditionalTempPathsJob(StrolchAgent agent, String id, String name, JobMode jobMode) {
		super(agent, id, name, jobMode);
	}

	public ClearAdditionalTempPathsJob(StrolchAgent agent, JobMode jobMode, long initialDelay, TimeUnit initialDelayTimeUnit,
			long delay, TimeUnit delayTimeUnit) {
		super(agent, ClearAdditionalTempPathsJob.class.getSimpleName(), ClearAdditionalTempPathsJob.class.getSimpleName(), jobMode);
		setDelay(initialDelay, initialDelayTimeUnit, delay, delayTimeUnit);
	}

	@Override
	protected void execute(PrivilegeContext ctx) throws Exception {
		RuntimeConfiguration runtimeConfiguration = getAgent().getStrolchConfiguration().getRuntimeConfiguration();
		List<String> pathIds = runtimeConfiguration.getClearTempPathIds();
		if (pathIds.isEmpty()) {
			logger.info("No clear temp path IDs configured.");
			return;
		}

		boolean verbose = runtimeConfiguration.isVerbose();
		boolean deleteEnabled = runtimeConfiguration.isTempRetentionDeleteEnabled();

		for (String pathId : pathIds) {
			File tempPath = runtimeConfiguration.getClearTempPath(pathId);
			if (tempPath == null) {
				logger.warn("Clear temp path for id {} is not configured!", pathId);
				continue;
			}
			if (!tempPath.exists()) {
				logger.info("Clear temp path {} (id: {}) does not exist, nothing to clear.", tempPath.getAbsolutePath(), pathId);
				continue;
			}

			if (deleteEnabled)
				logger.info("Clearing old data in clear temp path {} (id: {})...", tempPath.getAbsolutePath(), pathId);
			else
				logger.info("Simulating clearing of old data in clear temp path {} (id: {})...", tempPath.getAbsolutePath(), pathId);

			Stats stats = new Stats();
			Duration retention = runtimeConfiguration.getClearTempPathRetention(pathId);
			int keep = runtimeConfiguration.getClearTempPathKeep(pathId);

			clearOldFiles(tempPath, retention, keep, verbose, deleteEnabled, stats);
			cleanupEmptyDirectories(tempPath, verbose, deleteEnabled, stats);

			if (stats.deletedFiles > 0 || stats.deletedDirs > 0 || stats.failedDeletions > 0) {
				if (deleteEnabled) {
					logger.info("Cleared {} files and {} empty directories in clear temp path {}. ({} failed deletions)",
							stats.deletedFiles, stats.deletedDirs, tempPath.getAbsolutePath(), stats.failedDeletions);
				} else {
					logger.info("Simulation: would have cleared {} files and {} empty directories in clear temp path {}.",
							stats.deletedFiles, stats.deletedDirs, tempPath.getAbsolutePath());
				}
			} else {
				logger.info("No old data to clear in clear temp path {}.", tempPath.getAbsolutePath());
			}
		}
	}

	private void clearOldFiles(File dir, Duration retention, int keep, boolean verbose, boolean deleteEnabled,
			Stats stats) {
		List<File> allFiles = new ArrayList<>();
		findFilesRecursive(dir, allFiles);

		if (allFiles.size() <= keep)
			return;

		allFiles.sort(Comparator.comparingLong(File::lastModified).reversed());

		for (int i = keep; i < allFiles.size(); i++) {
			clearIfOld(allFiles.get(i), retention, verbose, deleteEnabled, stats);
		}
	}

	private void findFilesRecursive(File dir, List<File> allFiles) {
		File[] children = dir.listFiles();
		if (children == null)
			return;
		for (File child : children) {
			if (child.isDirectory())
				findFilesRecursive(child, allFiles);
			else
				allFiles.add(child);
		}
	}

	private void cleanupEmptyDirectories(File dir, boolean verbose, boolean deleteEnabled, Stats stats) {
		File[] children = dir.listFiles();
		if (children == null)
			return;
		for (File child : children) {
			if (child.isDirectory()) {
				cleanupEmptyDirectories(child, verbose, deleteEnabled, stats);
				deleteIfEmpty(child, verbose, deleteEnabled, stats);
			}
		}
	}

	private void clearIfOld(File file, Duration retention, boolean verbose, boolean deleteEnabled, Stats stats) {
		Instant lastModified = Instant.ofEpochMilli(file.lastModified());
		Instant threshold = Instant.now().minus(retention);
		if (lastModified.isBefore(threshold)) {
			if (deleteEnabled) {
				if (file.delete()) {
					stats.deletedFiles++;
					if (verbose)
						logger.info("Deleted old temp file {}", file.getAbsolutePath());
				} else {
					stats.failedDeletions++;
					logger.warn("Failed to delete old temp file {}", file.getAbsolutePath());
				}
			} else {
				stats.deletedFiles++;
				logger.info("Would delete old temp file {}", file.getAbsolutePath());
			}
		}
	}

	private void deleteIfEmpty(File dir, boolean verbose, boolean deleteEnabled, Stats stats) {
		File[] listFiles = dir.listFiles();
		if (listFiles != null && listFiles.length == 0) {
			if (deleteEnabled) {
				if (dir.delete()) {
					stats.deletedDirs++;
					if (verbose)
						logger.info("Deleted empty temp directory {}", dir.getAbsolutePath());
				}
			} else {
				stats.deletedDirs++;
				logger.info("Would delete empty temp directory {}", dir.getAbsolutePath());
			}
		}
	}

	private static class Stats {
		int deletedFiles;
		int deletedDirs;
		int failedDeletions;
	}
}
