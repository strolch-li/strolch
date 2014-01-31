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

import java.text.MessageFormat;
import java.util.List;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.Tags;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.inmemory.InMemoryTransaction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractTransaction implements StrolchTransaction {

	protected static final Logger logger = LoggerFactory.getLogger(InMemoryTransaction.class);
	private StrolchRealm realm;

	/**
	 * @param realm
	 */
	public AbstractTransaction(StrolchRealm realm) {
		this.realm = realm;
	}

	/**
	 * @return the realm
	 */
	protected StrolchRealm getRealm() {
		return this.realm;
	}

	@Override
	public ResourceMap getResourceMap() {
		return this.realm.getResourceMap();
	}

	@Override
	public OrderMap getOrderMap() {
		return this.realm.getOrderMap();
	}

	@Override
	public List<Order> doQuery(OrderQuery query) {
		return getPersistenceHandler().getOrderDao(this).doQuery(query);
	}

	@Override
	public List<Resource> doQuery(ResourceQuery query) {
		return getPersistenceHandler().getResourceDao(this).doQuery(query);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends StrolchElement> T findElement(Locator locator) {

		if (locator.getSize() < 3) {
			String msg = "The locator is invalid as it does not have at least three path elements (e.g. Resource/MyType/@id): {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, locator.toString());
			throw new StrolchException(msg);
		} else if (locator.getSize() > 5) {
			// TODO handle state variables, which will probably be separated by an additional part so we can differentiate between parameters and state variables on a parameter bag
			String msg = "The locator is invalid as it has more than 5 parts. The fifth part references a Parameter, which is the deepest fetchable entry: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, locator.toString());
			throw new StrolchException(msg);
		}

		List<String> elements = locator.getPathElements();
		GroupedParameterizedElement groupedParameterizedElement;
		String objectClassType = elements.get(0);
		String type = elements.get(1);
		String id = elements.get(2);
		switch (objectClassType) {
		case Tags.RESOURCE:
			groupedParameterizedElement = getResourceMap().getBy(this, type, id);
			break;
		case Tags.ORDER:
			groupedParameterizedElement = getOrderMap().getBy(this, type, id);
			break;
		default:
			throw new StrolchException(MessageFormat.format("Unknown object class {0}", objectClassType)); //$NON-NLS-1$
		}

		if (groupedParameterizedElement == null) {
			String msg = "No top level object could be found with locator {0}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, locator));
		}

		if (elements.size() == 3)
			return (T) groupedParameterizedElement;

		String parameterBagId = elements.get(3);
		ParameterBag bag = groupedParameterizedElement.getParameterBag(parameterBagId);
		if (bag == null) {
			String msg = "Could not find ParameterBag for locator {0} on element {1}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, locator, groupedParameterizedElement.getLocator()));
		}

		if (elements.size() == 4)
			return (T) bag;

		String parameterId = elements.get(4);
		Parameter<?> parameter = bag.getParameter(parameterId);
		return (T) parameter;
	}
}
