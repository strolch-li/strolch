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
package li.strolch.agent.api;

import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchRealm {

	public String getRealm();

	public void lock(StrolchRootElement element);

	public void unlock(StrolchRootElement lockedElement);

	public void releaseLock(StrolchRootElement lockedElement);

	public DataStoreMode getMode();

	public StrolchTransaction openTx(Certificate certificate, Class<?> clazz);

	public StrolchTransaction openTx(Certificate certificate, String action);

	public boolean isAuditTrailEnabledForRead();

	public boolean isAuditTrailEnabled();

	public boolean isUpdateObservers();

	public ObserverHandler getObserverHandler();
}
