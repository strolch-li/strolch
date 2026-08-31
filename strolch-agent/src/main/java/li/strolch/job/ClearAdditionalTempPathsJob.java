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
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * A job to clear arbitrary temporary paths defined in the {@link RuntimeConfiguration}.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ClearAdditionalTempPathsJob extends ClearTempPathJob {

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
			logStats(logger, tempPath, stats, deleteEnabled);
		}
	}
}
