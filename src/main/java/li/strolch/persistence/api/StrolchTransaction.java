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

import java.util.List;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;

public interface StrolchTransaction extends AutoCloseable {

	public String getRealmName();

	public void setCloseStrategy(TransactionCloseStrategy closeStrategy);

	public void autoCloseableCommit();

	public void autoCloseableRollback();

	@Override
	public void close() throws StrolchPersistenceException;

	public boolean isOpen();

	public ResourceMap getResourceMap();

	public OrderMap getOrderMap();

	public PersistenceHandler getPersistenceHandler();

	public List<Order> doQuery(OrderQuery query);

	public List<Resource> doQuery(ResourceQuery query);

	/**
	 * <p>
	 * Used to find a {@link StrolchElement} by a {@link Locator}.
	 * </p>
	 * 
	 * <p>
	 * A Locator has the form <i>&lt;ObjectClassType&gt;/&lt;Type&gt;/&lt;Id&gt;</i> - this is the least amount of path
	 * elements to find an object. Thus to query a {@link Resource} of type "MyType" and the id "@1" use the following
	 * path: <i>Resourcee/MyType/@1</i>
	 * </p>
	 * 
	 * <p>
	 * This method can also be used to find a deeper element, e.g. a specific {@link Parameter} on an
	 * {@link ParameterBag} on an {@link Order}. This would be done as follows: <i>Order/MyType/@1/myParam</i>
	 * </p>
	 * 
	 * @param locator
	 *            the locator defining the path to the element which is to be found
	 * 
	 * @return the element described by the locator
	 * 
	 * @throws StrolchException
	 *             if the element could not be found
	 * @throws ClassCastException
	 *             if the querying code is not asking for the correct instance. Do not query a {@link Parameter} if the
	 *             variable to which the result is to be is stored is a {@link Resource}, etc.
	 */
	public <T extends StrolchElement> T findElement(Locator locator) throws StrolchException, ClassCastException;
}
