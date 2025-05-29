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

package li.strolch.agent.impl.eclipsestorage;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.impl.ElementMapHelpers;
import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.model.parameter.Parameter;
import org.eclipse.store.storage.types.StorageManager;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ORDER_REF;

public class EclipseStorageOrderMap extends EclipseStorageElementMap<Order> implements OrderMap {

	public EclipseStorageOrderMap(String realm, StorageManager storageManager) {
		super(realm, Tags.ORDER, storageManager);
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ORDER_REF, refP);
	}
}
