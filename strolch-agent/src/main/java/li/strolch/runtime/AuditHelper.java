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

package li.strolch.runtime;

import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.search.StrolchSearch;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;

import static java.util.ResourceBundle.getBundle;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.model.log.LogMessageState.Information;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

public class AuditHelper {

	private static final Logger logger = LoggerFactory.getLogger(AuditHelper.class);

	public static void writeAuditForService(StrolchAgent agent, ServiceArgument arg, Certificate certificate,
			ServiceResult result, String realmName, String username, String svcName) {

		StrolchRealm realm = agent.getComponent(RealmHandler.class).getRealm(realmName);
		if (!realm.isAuditTrailEnabled())
			return;

		try {
			agent.runAsAgent(ctx -> {
				try (StrolchTransaction tx = realm.openTx(ctx.certificate(), ServiceHandler.class.getSimpleName(),
						false)) {
					Audit audit = new Audit();

					audit.setId(StrolchAgent.getUniqueIdLong());
					audit.setUsername(username);
					audit.setFirstname(certificate.getFirstname() == null ? certificate.getUsername() :
							certificate.getFirstname());
					audit.setLastname(
							certificate.getLastname() == null ? certificate.getUsername() : certificate.getLastname());
					audit.setDate(new Date());

					audit.setElementType(Service.class.getSimpleName());
					audit.setElementSubType(svcName);
					audit.setElementAccessed("");

					audit.setAccessType(AccessType.EXECUTE);
					audit.setAction(result.getState().name());

					audit.setAdditionalData(arg.toJson());

					tx.getAuditTrail().add(tx, audit);
					tx.commitOnClose();
				}
			});
		} catch (Exception e) {
			logger.error("Failed to log audit for service {}!", svcName, e);
			if (agent.hasComponent(OperationsLog.class)) {
				agent
						.getComponent(OperationsLog.class)
						.addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
								Locator.valueOf(AGENT, "strolch-agent", getUniqueId()), LogSeverity.Exception,
								Information, getBundle("strolch-agent"), "agent.service.audit.failed")
								.value("service", svcName)
								.value("user", username)
								.value("reason", e.getMessage())
								.withException(e));
			}
		}
	}

	public static void writeAuditForSearch(StrolchAgent agent, Certificate certificate, String realmName,
			String searchName) {

		StrolchRealm realm = agent.getRealm(realmName);
		try {
			agent.runAsAgent(ctx -> {
				try (StrolchTransaction tx = realm.openTx(ctx.certificate(), StrolchSearch.class.getSimpleName(),
						false)) {
					Audit audit = new Audit();

					audit.setId(StrolchAgent.getUniqueIdLong());
					audit.setUsername(certificate.getUsername());
					audit.setFirstname(certificate.getFirstname() == null ? certificate.getUsername() :
							certificate.getFirstname());
					audit.setLastname(
							certificate.getLastname() == null ? certificate.getUsername() : certificate.getLastname());
					audit.setDate(new Date());

					audit.setElementType(StrolchSearch.class.getSimpleName());
					audit.setElementSubType(searchName);
					audit.setElementAccessed("");

					audit.setAccessType(AccessType.EXECUTE);
					audit.setAction("");

					tx.getAuditTrail().add(tx, audit);
					tx.commitOnClose();
				}
			});
		} catch (Exception e) {
			logger.error("Failed to log audit for service {}!", searchName, e);
			if (agent.hasComponent(OperationsLog.class)) {
				agent
						.getComponent(OperationsLog.class)
						.addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
								Locator.valueOf(AGENT, "strolch-agent", getUniqueId()), LogSeverity.Exception,
								Information, getBundle("strolch-agent"), "agent.search.audit.failed")
								.value("search", searchName)
								.value("user", certificate.getUsername())
								.value("reason", e.getMessage())
								.withException(e));
			}
		}
	}
}
