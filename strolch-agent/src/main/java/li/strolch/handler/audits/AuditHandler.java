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

package li.strolch.handler.audits;

import com.google.gson.JsonObject;
import li.strolch.agent.api.*;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.search.StrolchSearch;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import org.jetbrains.annotations.NotNull;

import java.time.ZonedDateTime;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.*;

import static java.util.ResourceBundle.getBundle;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.model.log.LogMessageState.Information;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

public class AuditHandler extends StrolchComponent {

	private static final String className = AuditHandler.class.getSimpleName();

	private boolean run;
	private LinkedBlockingQueue<AuditTask> queue;
	private ExecutorService executorService;
	private Future<?> pruneTask;

	private Map<String, Long> seenHashes;
	private long lastSeenHashesPruning;
	private int lastSeenHashIntervalSeconds;

	public AuditHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.lastSeenHashIntervalSeconds = configuration.getInt("lastSeenHashIntervalSeconds", 60);
		this.seenHashes = new ConcurrentHashMap<>();
		this.lastSeenHashesPruning = System.currentTimeMillis();
		this.queue = new LinkedBlockingQueue<>();
		this.executorService = getSingleThreadExecutor(className);
		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {
		this.run = true;
		this.pruneTask = this.executorService.submit(this::handleQueue);
		super.start();
	}

	private void handleQueue() {
		while (this.run) {
			try {

				// prune hashes
				try {
					pruneHashes();
				} catch (Exception e) {
					logger.error("Failed to prune hashes", e);
				}

				// now handle add tasks
				AuditTask poll = this.queue.poll(1, TimeUnit.SECONDS);
				if (poll == null)
					continue;

				poll.run();

			} catch (InterruptedException e) {
				if (!this.run)
					logger.warn("Interrupted!");
				else
					logger.error("Failed to perform a task", e);
			} catch (Exception e) {
				logger.error("Failed to perform a task", e);
			}
		}
	}

	@Override
	public void stop() throws Exception {
		this.run = false;
		if (this.pruneTask != null)
			this.pruneTask.cancel(true);
		if (this.executorService != null)
			this.executorService.shutdownNow();
		if (this.seenHashes != null)
			this.seenHashes.clear();
		super.stop();
	}

	private void pruneHashes() {
		if (this.seenHashes.isEmpty())
			return;

		long now = System.currentTimeMillis();
		if (now - this.lastSeenHashesPruning < TimeUnit.MINUTES.toMillis(10))
			return;

		Set<String> hashes = new HashSet<>(this.seenHashes.keySet());
		for (String hash : hashes) {
			long sentTime = this.seenHashes.get(hash);
			if (now - sentTime > TimeUnit.MINUTES.toMillis(10)) {
				this.seenHashes.remove(hash);
			}
		}

		this.lastSeenHashesPruning = now;
	}

	public void writeAuditForServiceAsync(ServiceArgument arg, Certificate certificate, ServiceResult result,
			String realmName, String svcName) {
		if (this.queue != null)
			this.queue.add(() -> _writeAuditForService(arg, certificate, result, realmName, svcName));
	}

	private void _writeAuditForService(ServiceArgument arg, Certificate certificate, ServiceResult result,
			String realmName, String svcName) {
		try {
			// we currently don't want to create audits for the agent user
			if (certificate.isSystemUser())
				return;
			StrolchRealm realm = getComponent(RealmHandler.class).getRealm(realmName);
			if (!realm.isAuditTrailEnabled())
				return;

			addAudit(realm, buildServiceAudit(arg, certificate, result, svcName));

		} catch (Exception e) {
			logger.error("Failed to log audit for service {}!", svcName, e);
			if (hasComponent(OperationsLog.class)) {
				getComponent(OperationsLog.class).addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
						Locator.valueOf(AGENT, "strolch-agent", getUniqueId()), LogSeverity.Exception, Information,
						getBundle("strolch-agent"), "agent.service.audit.failed")
						.value("service", svcName)
						.value("user", certificate.getUsername())
						.value("reason", e.getMessage())
						.withException(e));
			}
		}
	}

	public void writeAuditForSearchAsync(Certificate certificate, String realmName, String searchName) {
		if (this.queue != null)
			this.queue.add(() -> _writeAuditForSearch(certificate, realmName, searchName));
	}

	private void _writeAuditForSearch(Certificate certificate, String realmName, String searchName) {
		try {
			// we currently don't want to create audits for the agent user
			if (certificate.isSystemUser())
				return;
			StrolchRealm realm = getComponent(RealmHandler.class).getRealm(realmName);
			if (!realm.isAuditTrailEnabled())
				return;

			addAudit(realm, buildSearchAudit(certificate, searchName));

		} catch (Exception e) {
			logger.error("Failed to log audit for service {}!", searchName, e);
			if (hasComponent(OperationsLog.class)) {
				getComponent(OperationsLog.class).addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
						Locator.valueOf(AGENT, "strolch-agent", getUniqueId()), LogSeverity.Exception, Information,
						getBundle("strolch-agent"), "agent.search.audit.failed")
						.value("search", searchName)
						.value("user", certificate.getUsername())
						.value("reason", e.getMessage())
						.withException(e));
			}
		}
	}

	public void writeAuditForApiCallAsync(Certificate certificate, String url, String method,
			JsonObject additionalData) {
		if (this.queue != null)
			this.queue.add(() -> _writeAuditForApiCall(certificate, url, method, additionalData));
	}

	private void _writeAuditForApiCall(Certificate certificate, String url, String method, JsonObject additionalData) {

		String realmName = certificate.getRealmOrDefault();
		try {
			// we currently don't want to create audits for the agent user
			if (certificate.isSystemUser())
				return;
			StrolchRealm realm = getComponent(RealmHandler.class).getRealm(realmName);
			if (!realm.isAuditTrailEnabled())
				return;

			addAudit(realm, buildApiCallAudit(certificate, url, method, additionalData));

		} catch (Exception e) {
			logger.error("Failed to log audit for REST API {}!", url, e);
			if (hasComponent(OperationsLog.class)) {
				getComponent(OperationsLog.class).addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
						Locator.valueOf(AGENT, "strolch-agent", getUniqueId()), LogSeverity.Exception, Information,
						getBundle("strolch-agent"), "agent.rest_api.audit.failed")
						.value("user", certificate.getUsername())
						.value("reason", e.getMessage())
						.withException(e));
			}
		}
	}

	private void addAudit(StrolchRealm realm, Audit audit) throws Exception {

		String hash = audit.buildRelevantHash();
		long now = System.currentTimeMillis();
		Long sentTime = this.seenHashes.get(hash);
		if (sentTime != null) {
			if (now - sentTime < TimeUnit.SECONDS.toMillis(this.lastSeenHashIntervalSeconds)) {
				// ignore audit we added not long ago
				if (logger.isDebugEnabled())
					logger.warn("Audit {} as already been sent less than 1min ago as hash is already known. Ignoring.",
							audit);
				return;
			}
		}
		this.seenHashes.put(hash, now);

		runAsAgent(ctx -> {
			try (StrolchTransaction tx = realm.openTx(ctx.certificate(), className, false)) {
				tx.add(audit);
				tx.commitOnClose();
			}
		});
	}

	@NotNull
	private static Audit buildServiceAudit(ServiceArgument arg, Certificate certificate, ServiceResult result,
			String svcName) {
		Audit audit = new Audit();

		audit.setId(StrolchAgent.getUniqueIdLong());
		audit.setUsername(certificate.getUsername());
		audit.setDate(ZonedDateTime.now());

		audit.setElementType(Service.class.getSimpleName());
		audit.setElementSubType(svcName);
		audit.setElementAccessed("");

		audit.setAccessType(AccessType.EXECUTE);
		audit.setAction(result.getState().name());

		audit.setAdditionalDataAsJson(arg.toJson());
		return audit;
	}

	private static Audit buildSearchAudit(Certificate certificate, String searchName) {
		Audit audit = new Audit();

		audit.setId(StrolchAgent.getUniqueIdLong());
		audit.setUsername(certificate.getUsername());
		audit.setDate(ZonedDateTime.now());

		audit.setElementType(StrolchSearch.class.getSimpleName());
		audit.setElementSubType(searchName);
		audit.setElementAccessed("");

		audit.setAccessType(AccessType.EXECUTE);
		audit.setAction("");
		return audit;
	}

	private Audit buildApiCallAudit(Certificate certificate, String url, String method, JsonObject additionalData) {
		Audit audit = new Audit();
		audit.setId(StrolchAgent.getUniqueIdLong());
		audit.setUsername(certificate.getUsername());
		audit.setDate(ZonedDateTime.now());

		audit.setElementType("REST_API");
		audit.setElementSubType(url);
		audit.setElementAccessed("");

		audit.setAccessType(AccessType.EXECUTE);
		audit.setAction(method);

		audit.setAdditionalDataAsJson(additionalData);
		return audit;
	}

	private interface AuditTask {
		void run();
	}
}
