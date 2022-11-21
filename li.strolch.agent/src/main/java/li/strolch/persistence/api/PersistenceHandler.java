/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.persistence.api;

import li.strolch.agent.api.*;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.audit.Audit;
import li.strolch.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PersistenceHandler {

	/**
	 * Returns true if this persistence layer supports paging, i.e. the DAO methods with a limit and offset may be used
	 */
	boolean supportsPaging();

	/**
	 * Opens a {@link StrolchTransaction} on the given {@link StrolchRealm}. The transaction is the main object to be
	 * used when accessing and modifying the elements of a realm.
	 *
	 * @param realm
	 * 		the realm for which the transaction is to be opened
	 * @param certificate
	 * 		the certificate which has access to the realm
	 * @param action
	 * 		the name of the transaction used for auditing
	 * @param readOnly
	 * 		if this TX is read-only
	 *
	 * @return the newly created {@link StrolchTransaction}
	 */
	StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action, boolean readOnly);

	/**
	 * Returns the {@link OrderDao} for the given transaction. Use this only if you want to bypass certain transaction
	 * features. Accessing {@link Order Orders} should be done through the {@link OrderMap} accessed from the
	 * transaction
	 *
	 * @param tx
	 * 		the transaction for which the {@link OrderDao} is to be returned
	 *
	 * @return the {@link OrderDao}
	 */
	OrderDao getOrderDao(StrolchTransaction tx);

	/**
	 * Returns the {@link ResourceDao} for the given transaction. Use this only if you want to bypass certain
	 * transaction features. Accessing {@link Resource Resources} should be done through the {@link ResourceMap}
	 * accessed from the transaction
	 *
	 * @param tx
	 * 		the transaction for which the {@link ResourceDao} is to be returned
	 *
	 * @return the {@link ResourceDao}
	 */
	ResourceDao getResourceDao(StrolchTransaction tx);

	/**
	 * Returns the {@link ActivityDao} for the given transaction. Use this only if you want to bypass certain
	 * transaction features. Accessing {@link Activity Activities} should be done through the {@link ActivityMap}
	 * accessed from the transaction
	 *
	 * @param tx
	 * 		the transaction for which the {@link ActivityDao} is to be returned
	 *
	 * @return the {@link ActivityDao}
	 */
	ActivityDao getActivityDao(StrolchTransaction tx);

	/**
	 * Returns the {@link AuditDao} for the given transaction. Use this only if you want to bypass certain transaction
	 * features. Accessing {@link Audit Audits} should be done through the {@link AuditTrail} accessed from the
	 * transaction
	 *
	 * @param tx
	 * 		the transaction for which the {@link AuditDao} is to be returned
	 *
	 * @return the {@link AuditDao}
	 */
	AuditDao getAuditDao(StrolchTransaction tx);

	/**
	 * Returns the {@link LogMessageDao} for the given transaction.
	 *
	 * @param tx
	 * 		the transaction for which the {@link LogMessageDao} is to be returned
	 *
	 * @return the {@link LogMessageDao}
	 */
	LogMessageDao getLogMessageDao(StrolchTransaction tx);
}
