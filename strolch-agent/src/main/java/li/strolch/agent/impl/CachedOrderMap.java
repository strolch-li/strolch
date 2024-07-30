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
package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ORDER_REF;

import java.time.ZonedDateTime;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchTransaction;

public class CachedOrderMap extends CachedElementMap<Order> implements OrderMap {

	public CachedOrderMap(StrolchRealm realm) {
		super(realm);
	}

	@Override
	public Order getTemplate(StrolchTransaction tx, String type, boolean assertExists) {
		Order template = super.getTemplate(tx, type, assertExists);
		template.setDate(ZonedDateTime.now());
		return template;
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ORDER_REF, refP);
	}

	@Override
	protected OrderDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getOrderDao(tx);
	}
}
