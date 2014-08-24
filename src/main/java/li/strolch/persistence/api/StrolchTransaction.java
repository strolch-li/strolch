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

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.OrderVisitor;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.api.Command;

public interface StrolchTransaction extends AutoCloseable {

	public String getRealmName();

	public void setCloseStrategy(TransactionCloseStrategy closeStrategy);

	public void autoCloseableCommit();

	public void autoCloseableRollback();

	@Override
	public void close() throws StrolchPersistenceException;

	public boolean isOpen();

	public boolean isRollingBack();

	public boolean isCommitting();

	public boolean isCommitted();

	public boolean isRolledBack();

	public AuditTrail getAuditTrail();

	public ResourceMap getResourceMap();

	public OrderMap getOrderMap();

	public PersistenceHandler getPersistenceHandler();

	public <T extends StrolchRootElement> void lock(T element);

	public <T extends StrolchRootElement> void unlock(T element);

	public void addCommand(Command command);

	public List<Order> doQuery(OrderQuery query);

	public <U> List<U> doQuery(OrderQuery query, OrderVisitor<U> orderVisitor);

	public List<Resource> doQuery(ResourceQuery query);

	public <U> List<U> doQuery(ResourceQuery query, ResourceVisitor<U> resourceVisitor);

	public List<Audit> doQuery(AuditQuery query);

	public <U> List<U> doQuery(AuditQuery query, AuditVisitor<U> auditVisitor);

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

	/**
	 * Returns the {@link Resource} which is referenced by the given {@link StringParameter}. A reference
	 * {@link Parameter} must have its interpretation set to {@link StrolchConstants#INTERPRETATION_RESOURCE_REF} and
	 * the UOM must be set to the resource's type and the value is the id of the resource
	 * 
	 * @param refP
	 *            the {@link StringParameter} which references a {@link Resource}
	 * @return the resource referenced by the parameter, or null if it does not exist
	 * 
	 * @throws StrolchException
	 *             if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	public Resource getResourceBy(StringParameter refP) throws StrolchException;

	/**
	 * Returns the {@link Order} which is referenced by the given {@link StringParameter}. A reference {@link Parameter}
	 * must have its interpretation set to {@link StrolchConstants#INTERPRETATION_ORDER_REF} and the UOM must be set to
	 * the order's type and the value is the id of the order
	 * 
	 * @param refP
	 *            the {@link StringParameter} which references an {@link Order}
	 * @return the order referenced by the parameter, or null if it does not exist
	 * 
	 * @throws StrolchException
	 *             if the {@link StringParameter} is not a properly configured as a reference parameter
	 */
	public Order getOrderBy(StringParameter refP) throws StrolchException;
}
