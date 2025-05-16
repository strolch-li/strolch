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

import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.Executors.*;

public class ExecutorPool {

	private static final Logger logger = LoggerFactory.getLogger(ExecutorPool.class);

	private final Map<String, ExecutorService> executors;
	private final Map<String, ScheduledExecutorService> scheduledExecutors;

	public ExecutorPool() {
		this.executors = new ConcurrentHashMap<>();
		this.scheduledExecutors = new ConcurrentHashMap<>();
	}

	public ExecutorService getExecutor(String poolName) {
		DBC.PRE.assertNotEmpty("poolName must be set!", poolName);
		return this.executors.computeIfAbsent(poolName, p -> newCachedThreadPool(new NamedThreadPoolFactory(p)));
	}

	public ExecutorService getSingleThreadExecutor(String poolName) {
		DBC.PRE.assertNotEmpty("poolName must be set!", poolName);
		return this.executors.computeIfAbsent(poolName, p -> newSingleThreadExecutor(new NamedThreadPoolFactory(p)));
	}

	public ScheduledExecutorService getScheduledExecutor(String poolName) {
		DBC.PRE.assertNotEmpty("poolName must be set!", poolName);
		return this.scheduledExecutors.computeIfAbsent(poolName,
				p -> newScheduledThreadPool(4, new NamedThreadPoolFactory(p)));
	}

	public void destroy() {
		this.executors.forEach(this::shutdownExecutor);
		this.scheduledExecutors.forEach(this::shutdownExecutor);
	}

	private void shutdownExecutor(String name, ExecutorService executor) {
		logger.info("Shutting down executor pool {}", name);
		try {
			List<Runnable> tasks = executor.shutdownNow();
			if (!tasks.isEmpty()) {
				logger.warn("The following {} tasks were never started for executor {} :", tasks.size(), name);
				for (Runnable runnable : tasks) {
					logger.warn("  {}", runnable);
				}
			}

			if (!executor.awaitTermination(5, TimeUnit.SECONDS))
				logger.error("Executor {} did not stop after " + 5 + "s!", name);
		} catch (InterruptedException e) {
			logger.error("Was interrupted while shutting down tasks");
		}
	}
}
