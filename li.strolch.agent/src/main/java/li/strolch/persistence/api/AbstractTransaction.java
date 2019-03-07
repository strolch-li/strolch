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

import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.model.Tags.AGENT;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Stream;

import li.strolch.agent.api.*;
import li.strolch.agent.impl.*;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.query.*;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.runtime.privilege.TransactedRestrictable;
import li.strolch.service.api.Command;
import li.strolch.utils.collections.MapOfMaps;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.objectfilter.ObjectFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractTransaction implements StrolchTransaction {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractTransaction.class);

	private ComponentContainer container;
	private PrivilegeHandler privilegeHandler;
	private OperationsLog operationsLog;

	private InternalStrolchRealm realm;

	private ObjectFilter objectFilter;
	private MapOfMaps<String, String, Resource> resourceCache;
	private MapOfMaps<String, String, Order> orderCache;
	private MapOfMaps<String, String, Activity> activityCache;

	private TransactionCloseStrategy closeStrategy;
	private boolean suppressUpdates;
	private boolean suppressAudits;
	private boolean suppressAuditsForAudits;
	private boolean suppressDoNothingLogging;
	private TransactionResult txResult;

	private List<Command> commands;
	private List<Command> flushedCommands;
	private Set<Locator> lockedElements;

	private AuditingOrderMap orderMap;
	private AuditingResourceMap resourceMap;
	private AuditingActivityMap activityMap;
	private AuditingAuditMapFacade auditTrail;

	private String action;
	private Certificate certificate;
	private PrivilegeContext privilegeContext;

	public AbstractTransaction(ComponentContainer container, StrolchRealm realm, Certificate certificate,
			String action) {
		DBC.PRE.assertNotNull("container must be set!", container); //$NON-NLS-1$
		DBC.PRE.assertNotNull("realm must be set!", realm); //$NON-NLS-1$
		DBC.PRE.assertNotNull("certificate must be set!", certificate); //$NON-NLS-1$
		DBC.PRE.assertNotNull("action must be set!", action); //$NON-NLS-1$

		this.container = container;
		this.privilegeHandler = container.getPrivilegeHandler();
		if (container.hasComponent(OperationsLog.class))
			this.operationsLog = container.getComponent(OperationsLog.class);
		this.realm = (InternalStrolchRealm) realm;
		this.action = action;
		this.certificate = certificate;

		this.commands = new ArrayList<>();
		this.flushedCommands = new ArrayList<>();
		this.lockedElements = new HashSet<>();
		this.closeStrategy = TransactionCloseStrategy.READ_ONLY;
		this.txResult = new TransactionResult(getRealmName(), System.nanoTime(), new Date());
		this.txResult.setState(TransactionState.OPEN);
	}

	public String getAction() {
		return this.action;
	}

	public TransactionResult getTxResult() {
		return this.txResult;
	}

	@Override
	public TransactionState getState() {
		return this.txResult.getState();
	}

	@Override
	public boolean isOpen() {
		return this.txResult.getState().isOpen();
	}

	@Override
	public boolean isRollingBack() {
		return this.txResult.getState().isRollingBack();
	}

	@Override
	public boolean isCommitting() {
		return this.txResult.getState().isCommitting();
	}

	@Override
	public boolean isClosing() {
		return this.txResult.getState().isClosing();
	}

	@Override
	public boolean isClosed() {
		return this.txResult.getState().isClosed();
	}

	@Override
	public boolean isFailed() {
		return this.txResult.getState().isFailed();
	}

	@Override
	public String getRealmName() {
		return this.realm.getRealm();
	}

	public StrolchRealm getRealm() {
		return this.realm;
	}

	@Override
	public Certificate getCertificate() {
		return certificate;
	}

	@Override
	public PrivilegeContext getPrivilegeContext() {
		if (this.privilegeContext == null) {
			this.privilegeContext = this.privilegeHandler.validate(this.certificate);
		}
		return this.privilegeContext;
	}

	@Override
	public TransactionCloseStrategy getCloseStrategy() {
		return this.closeStrategy;
	}

	private void setCloseStrategy(TransactionCloseStrategy closeStrategy) {
		this.closeStrategy = closeStrategy;
	}

	@Override
	public void close() throws StrolchTransactionException {
		this.closeStrategy.close(this);
	}

	@Override
	public void doNothingOnClose() {
		setCloseStrategy(TransactionCloseStrategy.READ_ONLY);
	}

	@Override
	public void commitOnClose() {
		setCloseStrategy(TransactionCloseStrategy.COMMIT);
	}

	@Override
	public void rollbackOnClose() {
		setCloseStrategy(TransactionCloseStrategy.ROLLBACK);
	}

	@Override
	public StrolchTransactionException fail(String string) {
		rollbackOnClose();
		return new StrolchTransactionException(string);
	}

	@Override
	public void setSuppressUpdates(boolean suppressUpdates) {
		this.suppressUpdates = suppressUpdates;
	}

	@Override
	public boolean isSuppressUpdates() {
		return this.suppressUpdates;
	}

	@Override
	public void setSuppressAudits(boolean suppressAudits) {
		this.suppressAudits = suppressAudits;
	}

	@Override
	public void setSuppressAuditsForAudits(boolean suppressAuditsForAudits) {
		this.suppressAuditsForAudits = suppressAuditsForAudits;
	}

	@Override
	public boolean isSuppressAudits() {
		return this.suppressAudits;
	}

	@Override
	public boolean isSuppressAuditsForAudits() {
		return this.suppressAuditsForAudits;
	}

	@Override
	public boolean isSuppressDoNothingLogging() {
		return suppressDoNothingLogging;
	}

	@Override
	public void setSuppressDoNothingLogging(boolean quietDoNothing) {
		this.suppressDoNothingLogging = quietDoNothing;
	}

	@Override
	public boolean isVersioningEnabled() {
		return this.realm.isVersioningEnabled();
	}

	@Override
	public boolean needsCommit() {
		return (this.objectFilter != null && !this.objectFilter.isEmpty()) || !this.commands.isEmpty()
				|| !this.flushedCommands.isEmpty();
	}

	@Override
	public void lock(Locator locator) throws StrolchLockException {
		this.realm.lock(locator);
		this.lockedElements.add(locator);
	}

	@Override
	public <T extends StrolchRootElement> void lock(T element) throws StrolchLockException {
		Locator locator = element.getLocator();
		this.realm.lock(locator);
		this.lockedElements.add(locator);
	}

	@Override
	public <T extends StrolchRootElement> void releaseLock(T element) throws StrolchLockException {
		Locator locator = element.getLocator();
		releaseLock(locator);
	}

	@Override
	public <T extends StrolchRootElement> void releaseLock(Locator locator) throws StrolchLockException {
		this.realm.releaseLock(locator);
		this.lockedElements.remove(locator);
	}

	private void releaseElementLocks() {
		for (Locator locator : this.lockedElements) {
			this.realm.releaseLock(locator);
		}
		this.lockedElements.clear();
	}

	@Override
	public void addCommand(Command command) {
		this.commands.add(command);
	}

	@Override
	public ResourceMap getResourceMap() {
		if (this.resourceMap == null) {
			this.resourceMap = new AuditingResourceMap(this.realm.getResourceMap(),
					this.realm.isAuditTrailEnabledForRead());
		}
		return this.resourceMap;
	}

	@Override
	public OrderMap getOrderMap() {
		if (this.orderMap == null) {
			this.orderMap = new AuditingOrderMap(this.realm.getOrderMap(), this.realm.isAuditTrailEnabledForRead());
		}
		return this.orderMap;
	}

	@Override
	public ActivityMap getActivityMap() {
		if (this.activityMap == null) {
			this.activityMap = new AuditingActivityMap(this.realm.getActivityMap(),
					this.realm.isAuditTrailEnabledForRead());
		}
		return this.activityMap;
	}

	@Override
	public AuditTrail getAuditTrail() {
		if (this.auditTrail == null) {
			this.auditTrail = new AuditingAuditMapFacade(this.realm.getAuditTrail(),
					this.realm.isAuditTrailEnabledForRead());
		}
		return this.auditTrail;
	}

	@Override
	public ComponentContainer getContainer() {
		return this.container;
	}

	private void assertQueryAllowed(StrolchQuery query) {
		try {
			getPrivilegeContext().validateAction(query);
		} catch (PrivilegeModelException e) {
			throw e;
		} catch (PrivilegeException e) {
			throw new StrolchAccessDeniedException(this.certificate, query, ExceptionHelper.getExceptionMessage(e), e);
		}
	}

	@Override
	public <U> List<U> doQuery(OrderQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("orderVisitor", query.getVisitor());
		return getOrderMap().doQuery(this, query);
	}

	@Override
	public <U> List<U> doQuery(ResourceQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("resourceVisitor", query.getVisitor());
		return getResourceMap().doQuery(this, query);
	}

	@Override
	public <U> List<U> doQuery(ActivityQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("activityVisitor", query.getVisitor());
		return getActivityMap().doQuery(this, query);
	}

	@Override
	public <U> List<U> doQuery(AuditQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("auditVisitor", query.getVisitor());
		return getAuditTrail().doQuery(this, query);
	}

	@Override
	public <T extends StrolchElement> T findElement(Locator locator) throws StrolchException, ClassCastException {
		return findElement(locator, false);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends StrolchElement> T findElement(Locator locator, boolean allowNull) {

		// Resource/<type>/<id>
		// Resource/<type>/<id>/Bag/<id>
		// Resource/<type>/<id>/Bag/<id>/<param_id>
		// Resource/<type>/<id>/State/<id>
		// Order/<type>/<id>
		// Order/<type>/<id>/Bag/<id>
		// Order/<type>/<id>/Bag/<id>/<param_id>

		if (locator.getSize() < 3) {
			String msg = "The locator is invalid as it does not have at least three path elements (e.g. Resource/MyType/@id): {0}"; //$NON-NLS-1$
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
			groupedParameterizedElement = getResourceBy(type, id);
			break;
		case Tags.ORDER:
			groupedParameterizedElement = getOrderBy(type, id);
			break;
		case Tags.ACTIVITY:
			groupedParameterizedElement = getActivityBy(type, id);
			break;
		default:
			throw new StrolchException(MessageFormat.format("Unknown object class {0}", objectClassType)); //$NON-NLS-1$
		}

		if (groupedParameterizedElement == null) {
			if (allowNull)
				return null;
			String msg = "No top level object could be found with locator {0}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, locator));
		}

		if (elements.size() == 3)
			return (T) groupedParameterizedElement;

		// state or bag
		String stateOrBagOrActivity = elements.get(3);
		if (stateOrBagOrActivity.equals(Tags.BAG)) {

			String parameterBagId = elements.get(4);
			ParameterBag bag = groupedParameterizedElement.getParameterBag(parameterBagId);
			if (bag == null) {
				if (allowNull)
					return null;
				String msg = "Could not find ParameterBag for locator {0} on element {1}"; //$NON-NLS-1$
				throw new StrolchException(
						MessageFormat.format(msg, locator, groupedParameterizedElement.getLocator()));
			}

			if (elements.size() == 5)
				return (T) bag;

			String parameterId = elements.get(5);
			Parameter<?> parameter = bag.getParameter(parameterId);
			if (parameter == null) {
				if (allowNull)
					return null;
				String msg = "Could not find Parameter for locator {0} on element {1}"; //$NON-NLS-1$
				throw new StrolchException(MessageFormat.format(msg, locator, bag.getLocator()));
			}
			return (T) parameter;

		} else if (stateOrBagOrActivity.equals(Tags.TIMED_STATE)) {

			if (elements.size() != 5) {
				String msg = "Missing state Id on locator {0}"; //$NON-NLS-1$
				throw new StrolchException(MessageFormat.format(msg, locator));
			}

			Resource resource = (Resource) groupedParameterizedElement;
			String stateId = elements.get(4);

			StrolchTimedState<IValue<?>> timedState = resource.getTimedState(stateId);
			return (T) timedState;

		} else if (groupedParameterizedElement instanceof Activity) {

			Activity activity = (Activity) groupedParameterizedElement;

			Iterator<String> iter = elements.subList(3, elements.size()).iterator();
			IActivityElement element = activity;
			while (iter.hasNext()) {
				String next = iter.next();

				if (!(element instanceof Activity)) {
					String msg = "Invalid locator {0} with part {1} as not an Activity but deeper element specified"; //$NON-NLS-1$
					throw new StrolchException(MessageFormat.format(msg, locator, next));
				}

				element = ((Activity) element).getElement(next);
			}

			return (T) element;
		}

		if (allowNull)
			return null;

		String msg = "Invalid locator {0} with part {1}"; //$NON-NLS-1$
		throw new StrolchException(MessageFormat.format(msg, locator, stateOrBagOrActivity));
	}

	@Override
	public <U, T extends Parameter<U>> Optional<T> findParameterOnHierarchy(StrolchRootElement element,
			String parentParamKey, String bagKey, String paramKey) {

		DBC.PRE.assertNotNull("element must not be null!", element);
		DBC.PRE.assertNotEmpty("parentParamKey must be set", parentParamKey);
		DBC.PRE.assertNotEmpty("bagKey must be set", bagKey);
		DBC.PRE.assertNotEmpty("paramKey must be set", paramKey);

		T t = element.getParameter(bagKey, paramKey);

		Set<StrolchRootElement> parents = new HashSet<>();

		StrolchRootElement parent = element;
		while (t == null) {
			StringParameter parentP = parent.getParameter(BAG_RELATIONS, parentParamKey);
			if (parentP == null || parentP.isEmpty())
				break;

			switch (parentP.getInterpretation()) {
			case StrolchConstants.INTERPRETATION_RESOURCE_REF:
				parent = getResourceBy(parentP);
				break;
			case StrolchConstants.INTERPRETATION_ORDER_REF:
				parent = getOrderBy(parentP);
				break;
			case StrolchConstants.INTERPRETATION_ACTIVITY_REF:
				parent = getActivityBy(parentP);
				break;
			default:
				throw new IllegalStateException("Unhandled element ref " + parentP.getInterpretation());
			}

			if (parent == null)
				break;
			if (!parents.add(parent))
				throw new IllegalStateException(
						"circular dependencies from " + element.getLocator() + " to " + parent.getLocator()
								+ " on relations parameter " + parentParamKey);

			t = parent.getParameter(bagKey, paramKey);
		}

		return Optional.ofNullable(t);
	}

	private <V> V getElementFromFilter(String key, Locator locator) {
		if (this.objectFilter == null)
			return null;
		return this.objectFilter.getElement(key, locator);
	}

	private boolean hasElementInFilter(String key, Locator locator) {
		return this.objectFilter != null && this.objectFilter.hasElement(key, locator);
	}

	@Override
	public Stream<Resource> streamResources(String... types) {
		return getResourceMap().stream(this, types).map(e -> {
			// perhaps the element was changed before, so we check if it is in the filter first
			Resource element = getElementFromFilter(Tags.RESOURCE, e.getLocator());
			return element == null ? e : element;
		});
	}

	@Override
	public Stream<Order> streamOrders(String... types) {
		return getOrderMap().stream(this, types).map(e -> {
			// perhaps the element was changed before, so we check if it is in the filter first
			Order element = getElementFromFilter(Tags.ORDER, e.getLocator());
			return element == null ? e : element;
		});
	}

	@Override
	public Stream<Activity> streamActivities(String... types) {
		return getActivityMap().stream(this, types).map(e -> {
			// perhaps the element was changed before, so we check if it is in the filter first
			Activity element = getElementFromFilter(Tags.ACTIVITY, e.getLocator());
			return element == null ? e : element;
		});
	}

	@Override
	public Resource getResourceTemplate(String type) {
		return getResourceTemplate(type, false);
	}

	@Override
	public Resource getResourceTemplate(String type, boolean assertExists) throws StrolchException {
		Resource element = getElementFromFilter(Tags.RESOURCE, Resource.locatorFor(StrolchConstants.TEMPLATE, type));
		if (element != null)
			return element;
		return getResourceMap().getTemplate(this, type, assertExists);
	}

	@Override
	public Order getOrderTemplate(String type) {
		return getOrderTemplate(type, false);
	}

	@Override
	public Order getOrderTemplate(String type, boolean assertExists) throws StrolchException {
		Order element = getElementFromFilter(Tags.ORDER, Order.locatorFor(StrolchConstants.TEMPLATE, type));
		if (element == null)
			element = getOrderMap().getTemplate(this, type, assertExists);
		return element;
	}

	@Override
	public Activity getActivityTemplate(String type) {
		return getActivityTemplate(type, false);
	}

	@Override
	public Activity getActivityTemplate(String type, boolean assertExists) throws StrolchException {
		Activity element = getElementFromFilter(Tags.ACTIVITY, Activity.locatorFor(StrolchConstants.TEMPLATE, type));
		if (element != null)
			return element;
		return getActivityMap().getTemplate(this, type, assertExists);
	}

	@Override
	public Order getOrderBy(String type, String id) {
		return getOrderBy(type, id, false);
	}

	@Override
	public Order getOrderBy(String type, String id, boolean assertExists) throws StrolchException {
		Order element = getElementFromFilter(Tags.ORDER, Order.locatorFor(type, id));
		if (element != null)
			return element;

		element = getCachedOrder(type, id);
		if (element != null)
			return element;

		element = getOrderMap().getBy(this, type, id, assertExists);
		if (element != null) {
			if (this.orderCache == null)
				this.orderCache = new MapOfMaps<>(1);
			this.orderCache.addElement(type, id, element);
		}

		return element;
	}

	@Override
	public Order getOrderBy(StringParameter refP) throws StrolchException {
		return getOrderBy(refP, false);
	}

	@Override
	public Order getOrderBy(StringParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ORDER_REF, refP);

		if (assertExists && refP.isEmpty()) {
			String msg = "The Order with type \"{0}\" and id \"{1}\" does not exist for param \"{2}\""; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getUom(), refP.getValue(), refP.getLocator()));
		}

		if (refP.isEmpty())
			return null;

		Order element = getElementFromFilter(Tags.ORDER, Order.locatorFor(refP.getUom(), refP.getValue()));
		if (element != null)
			return element;

		element = getCachedOrder(refP.getUom(), refP.getValue());
		if (element != null)
			return element;

		element = getOrderMap().getBy(this, refP, assertExists);
		if (element != null) {
			if (this.orderCache == null)
				this.orderCache = new MapOfMaps<>(1);
			this.orderCache.addElement(refP.getUom(), refP.getValue(), element);
		}

		return element;
	}

	@Override
	public List<Order> getOrdersBy(StringListParameter refP) throws StrolchException {
		return getOrdersBy(refP, false);
	}

	@Override
	public List<Order> getOrdersBy(StringListParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ORDER_REF, refP);
		if (refP.isEmpty())
			return new ArrayList<>();

		List<Order> elements = new ArrayList<>();
		for (String id : refP.getValue()) {
			Order element = getOrderBy(refP.getUom(), id, assertExists);
			if (element != null)
				elements.add(element);
		}
		return elements;
	}

	@Override
	public Resource getResourceBy(String type, String id) {
		return getResourceBy(type, id, false);
	}

	@Override
	public Resource getResourceBy(String type, String id, boolean assertExists) throws StrolchException {
		Resource element = getElementFromFilter(Tags.RESOURCE, Resource.locatorFor(type, id));
		if (element != null)
			return element;

		element = getCachedResource(type, id);
		if (element != null)
			return element;

		element = getResourceMap().getBy(this, type, id, assertExists);
		if (element != null) {
			if (this.resourceCache == null)
				this.resourceCache = new MapOfMaps<>(1);
			this.resourceCache.addElement(type, id, element);
		}

		return element;
	}

	@Override
	public Resource getResourceBy(StringParameter refP) throws StrolchException {
		return getResourceBy(refP, false);
	}

	@Override
	public Resource getResourceBy(StringParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_RESOURCE_REF, refP);

		if (assertExists && refP.isEmpty()) {
			String msg = "The Resource with type \"{0}\" and id \"{1}\" does not exist for param \"{2}\""; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getUom(), refP.getValue(), refP.getLocator()));
		}

		if (refP.isEmpty())
			return null;

		Resource element = getElementFromFilter(Tags.RESOURCE, Resource.locatorFor(refP.getUom(), refP.getValue()));
		if (element != null)
			return element;

		element = getCachedResource(refP.getUom(), refP.getValue());
		if (element != null)
			return element;

		element = getResourceMap().getBy(this, refP, assertExists);
		if (element != null) {
			if (this.resourceCache == null)
				this.resourceCache = new MapOfMaps<>(1);
			this.resourceCache.addElement(refP.getUom(), refP.getValue(), element);
		}

		return element;
	}

	@Override
	public List<Resource> getResourcesBy(StringListParameter refP) throws StrolchException {
		return getResourcesBy(refP, false);
	}

	@Override
	public List<Resource> getResourcesBy(StringListParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_RESOURCE_REF, refP);
		if (refP.isEmpty())
			return new ArrayList<>();

		List<Resource> elements = new ArrayList<>();
		for (String id : refP.getValue()) {
			Resource element = getResourceBy(refP.getUom(), id, assertExists);
			if (element != null)
				elements.add(element);
		}
		return elements;
	}

	@Override
	public Resource getResourceFor(Action action) throws StrolchException {
		return getResourceFor(action, false);
	}

	@Override
	public Resource getResourceFor(Action action, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotEmpty("action.resourceType must be set", action.getResourceType());
		DBC.PRE.assertNotEmpty("action.resourceId must be set", action.getResourceId());
		return getResourceBy(action.getResourceType(), action.getResourceId(), assertExists);
	}

	@Override
	public Activity getActivityBy(String type, String id) {
		return getActivityBy(type, id, false);
	}

	@Override
	public Activity getActivityBy(String type, String id, boolean assertExists) throws StrolchException {
		Activity element = getElementFromFilter(Tags.ACTIVITY, Activity.locatorFor(type, id));
		if (element != null)
			return element;

		element = getCachedActivity(type, id);
		if (element != null)
			return element;

		element = getActivityMap().getBy(this, type, id, assertExists);
		if (element != null) {
			if (this.activityCache == null)
				this.activityCache = new MapOfMaps<>(1);
			this.activityCache.addElement(type, id, element);
		}

		return element;
	}

	@Override
	public Activity getActivityBy(StringParameter refP) throws StrolchException {
		return getActivityBy(refP, false);
	}

	@Override
	public Activity getActivityBy(StringParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ACTIVITY_REF, refP);

		if (assertExists && refP.isEmpty()) {
			String msg = "The Activity with type \"{0}\" and id \"{1}\" does not exist for param \"{2}\""; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getUom(), refP.getValue(), refP.getLocator()));
		}

		if (refP.isEmpty())
			return null;

		Activity element = getElementFromFilter(Tags.ACTIVITY, Activity.locatorFor(refP.getUom(), refP.getValue()));
		if (element != null)
			return element;

		element = getCachedActivity(refP.getUom(), refP.getValue());
		if (element != null)
			return element;

		element = getActivityMap().getBy(this, refP, assertExists);
		if (element != null) {
			if (this.activityCache == null)
				this.activityCache = new MapOfMaps<>(1);
			this.activityCache.addElement(refP.getUom(), refP.getValue(), element);
		}

		return element;
	}

	@Override
	public List<Activity> getActivitiesBy(StringListParameter refP) throws StrolchException {
		return getActivitiesBy(refP, false);
	}

	@Override
	public List<Activity> getActivitiesBy(StringListParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ACTIVITY_REF, refP);
		if (refP.isEmpty())
			return new ArrayList<>();

		List<Activity> elements = new ArrayList<>();
		for (String id : refP.getValue()) {
			Activity element = getActivityBy(refP.getUom(), id, assertExists);
			if (element != null)
				elements.add(element);
		}
		return elements;
	}

	@Override
	public Resource getCachedResource(String type, String id) {
		if (this.resourceCache == null)
			return null;
		return this.resourceCache.getElement(type, id);
	}

	@Override
	public Order getCachedOrder(String type, String id) {
		if (this.orderCache == null)
			return null;
		return this.orderCache.getElement(type, id);
	}

	@Override
	public Activity getCachedActivity(String type, String id) {
		if (this.activityCache == null)
			return null;
		return this.activityCache.getElement(type, id);
	}

	@Override
	public boolean hasResource(String type, String id) {
		boolean inFilter = hasElementInFilter(Tags.RESOURCE, Resource.locatorFor(type, id));
		return inFilter || getResourceMap().hasElement(this, type, id);
	}

	@Override
	public boolean hasOrder(String type, String id) {
		boolean inFilter = hasElementInFilter(Tags.ORDER, Order.locatorFor(type, id));
		return inFilter || getOrderMap().hasElement(this, type, id);
	}

	@Override
	public boolean hasActivity(String type, String id) {
		boolean inFilter = hasElementInFilter(Tags.ACTIVITY, Activity.locatorFor(type, id));
		return inFilter || getActivityMap().hasElement(this, type, id);
	}

	private ObjectFilter getObjectFilter() {
		if (this.objectFilter == null)
			this.objectFilter = new ObjectFilter();
		return this.objectFilter;
	}

	@Override
	public void assertHasPrivilege(Operation operation, StrolchRootElement element) throws AccessDeniedException {
		DBC.PRE.assertNotNull("operation must not be null", operation);
		DBC.PRE.assertNotNull("element must not be null", element);
		getPrivilegeContext()
				.validateAction(new TransactedRestrictable(this, operation.getPrivilegeName(element), element));
	}

	@Override
	public void assertHasRole(String roleName) throws AccessDeniedException {
		DBC.PRE.assertNotNull("roleName must not be null", roleName);
		getPrivilegeContext().assertHasRole(roleName);
	}

	@Override
	public void assertHasAnyRole(String... roleNames) throws AccessDeniedException {
		DBC.PRE.assertNotNull("roleNames must not be null", roleNames);
		getPrivilegeContext().assertHasAnyRole(roleNames);
	}

	@Override
	public void addOrUpdate(Resource resource) throws StrolchModelException {
		DBC.PRE.assertNotNull("resource must not be null", resource);
		if (hasResource(resource.getType(), resource.getId()) && !getObjectFilter()
				.hasElement(Tags.RESOURCE, resource.getLocator()))
			getObjectFilter().update(Tags.RESOURCE, resource.getLocator(), resource);
		else
			getObjectFilter().add(Tags.RESOURCE, resource.getLocator(), resource);
	}

	@Override
	public void addOrUpdate(Order order) throws StrolchModelException {
		DBC.PRE.assertNotNull("order must not be null", order);
		if (hasOrder(order.getType(), order.getId()) && !getObjectFilter().hasElement(Tags.ORDER, order.getLocator()))
			getObjectFilter().update(Tags.ORDER, order.getLocator(), order);
		else
			getObjectFilter().add(Tags.ORDER, order.getLocator(), order);
	}

	@Override
	public void addOrUpdate(Activity activity) throws StrolchModelException {
		DBC.PRE.assertNotNull("activity must not be null", activity);
		if (hasActivity(activity.getType(), activity.getId()) && !getObjectFilter()
				.hasElement(Tags.ACTIVITY, activity.getLocator()))
			getObjectFilter().update(Tags.ACTIVITY, activity.getLocator(), activity);
		else
			getObjectFilter().add(Tags.ACTIVITY, activity.getLocator(), activity);
	}

	@Override
	public void add(Resource resource) throws StrolchModelException {
		DBC.PRE.assertNotNull("resource must not be null", resource);
		getObjectFilter().add(Tags.RESOURCE, resource.getLocator(), resource);
	}

	@Override
	public void add(Order order) throws StrolchException {
		DBC.PRE.assertNotNull("order must not be null", order);
		getObjectFilter().add(Tags.ORDER, order.getLocator(), order);
	}

	@Override
	public void add(Activity activity) throws StrolchException {
		DBC.PRE.assertNotNull("activity must not be null", activity);
		getObjectFilter().add(Tags.ACTIVITY, activity.getLocator(), activity);
	}

	@Override
	public void update(Resource resource) throws StrolchException {
		DBC.PRE.assertNotNull("resource must not be null", resource);
		getObjectFilter().update(Tags.RESOURCE, resource.getLocator(), resource);
	}

	@Override
	public void update(Order order) {
		DBC.PRE.assertNotNull("order must not be null", order);
		getObjectFilter().update(Tags.ORDER, order.getLocator(), order);
	}

	@Override
	public void update(Activity activity) throws StrolchException {
		DBC.PRE.assertNotNull("activity must not be null", activity);
		getObjectFilter().update(Tags.ACTIVITY, activity.getLocator(), activity);
	}

	@Override
	public void remove(Resource resource) throws StrolchException {
		DBC.PRE.assertNotNull("resource must not be null", resource);
		getObjectFilter().remove(Tags.RESOURCE, resource.getLocator(), resource);
		if (this.resourceCache != null) {
			this.resourceCache.removeElement(resource.getType(), resource.getId());
		}
	}

	@Override
	public void remove(Order order) throws StrolchException {
		DBC.PRE.assertNotNull("order must not be null", order);
		getObjectFilter().remove(Tags.ORDER, order.getLocator(), order);
		if (this.orderCache != null) {
			this.orderCache.removeElement(order.getType(), order.getId());
		}
	}

	@Override
	public void remove(Activity activity) throws StrolchException {
		DBC.PRE.assertNotNull("activity must not be null", activity);
		getObjectFilter().remove(Tags.ACTIVITY, activity.getLocator(), activity);
		if (this.activityCache != null) {
			this.activityCache.removeElement(activity.getType(), activity.getId());
		}
	}

	private void addModelChangeCommands() {
		if (this.objectFilter == null)
			return;

		List<Object> removed;
		List<Object> updated;
		List<Object> added;

		/*
		 * Resources
		 */
		removed = this.objectFilter.getRemoved(Tags.RESOURCE);
		if (!removed.isEmpty()) {
			for (Object obj : removed) {
				RemoveResourceCommand cmd = new RemoveResourceCommand(getContainer(), this);
				cmd.setResource((Resource) obj);
				addCommand(cmd);
			}
		}

		updated = this.objectFilter.getUpdated(Tags.RESOURCE);
		if (!updated.isEmpty()) {
			for (Object obj : updated) {
				UpdateResourceCommand cmd = new UpdateResourceCommand(getContainer(), this);
				cmd.setResource((Resource) obj);
				addCommand(cmd);
			}
		}

		added = this.objectFilter.getAdded(Tags.RESOURCE);
		if (!added.isEmpty()) {
			for (Object obj : added) {
				AddResourceCommand cmd = new AddResourceCommand(getContainer(), this);
				cmd.setResource((Resource) obj);
				addCommand(cmd);
			}
		}

		/*
		 * Orders
		 */
		removed = this.objectFilter.getRemoved(Tags.ORDER);
		if (!removed.isEmpty()) {
			for (Object obj : removed) {
				RemoveOrderCommand cmd = new RemoveOrderCommand(getContainer(), this);
				cmd.setOrder((Order) obj);
				addCommand(cmd);
			}
		}

		updated = this.objectFilter.getUpdated(Tags.ORDER);
		if (!updated.isEmpty()) {
			for (Object obj : updated) {
				UpdateOrderCommand cmd = new UpdateOrderCommand(getContainer(), this);
				cmd.setOrder((Order) obj);
				addCommand(cmd);
			}
		}

		added = this.objectFilter.getAdded(Tags.ORDER);
		if (!added.isEmpty()) {
			for (Object obj : added) {
				AddOrderCommand cmd = new AddOrderCommand(getContainer(), this);
				cmd.setOrder((Order) obj);
				addCommand(cmd);
			}
		}

		/*
		 * Activities
		 */
		removed = this.objectFilter.getRemoved(Tags.ACTIVITY);
		if (!removed.isEmpty()) {
			for (Object obj : removed) {
				RemoveActivityCommand cmd = new RemoveActivityCommand(getContainer(), this);
				cmd.setActivity((Activity) obj);
				addCommand(cmd);
			}
		}

		updated = this.objectFilter.getUpdated(Tags.ACTIVITY);
		if (!updated.isEmpty()) {
			for (Object obj : updated) {
				UpdateActivityCommand cmd = new UpdateActivityCommand(getContainer(), this);
				cmd.setActivity((Activity) obj);
				addCommand(cmd);
			}
		}

		added = this.objectFilter.getAdded(Tags.ACTIVITY);
		if (!added.isEmpty()) {
			for (Object obj : added) {
				AddActivityCommand cmd = new AddActivityCommand(getContainer(), this);
				cmd.setActivity((Activity) obj);
				addCommand(cmd);
			}
		}

		// clear, so that we don't do it twice in case of a flush()
		this.objectFilter.clearCache();
	}

	@Override
	public void clearCache() {
		if (this.orderCache != null) {
			this.orderCache.clear();
			this.orderCache = null;
		}
		if (this.resourceCache != null) {
			this.resourceCache.clear();
			this.resourceCache = null;
		}
		if (this.activityCache != null) {
			this.activityCache.clear();
			this.activityCache = null;
		}
	}

	@Override
	public void flush() {
		try {
			addModelChangeCommands();
			validateCommands();
			doCommands();
			writeChanges();
			clearCache();
		} catch (Exception e) {
			this.closeStrategy = TransactionCloseStrategy.ROLLBACK;

			String msg = "Strolch Transaction for realm {0} failed due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getRealmName(), ExceptionHelper.getExceptionMessage(e));
			throw new StrolchTransactionException(msg, e);
		}
	}

	@Override
	public void autoCloseableCommit() {
		long start = System.nanoTime();

		try {
			this.txResult.setState(TransactionState.COMMITTING);

			addModelChangeCommands();
			validateCommands();
			doCommands();

			// do it twice, since some commands might generate new model changes
			if (this.objectFilter != null && !this.objectFilter.isEmpty()) {
				addModelChangeCommands();
				validateCommands();
				doCommands();
			}

			writeChanges();

			long auditTrailDuration = writeAuditTrail();
			long updateObserversDuration = updateObservers();

			// commit and close the connection
			commit();

			handleCommit(start, auditTrailDuration, updateObserversDuration);

			this.txResult.setState(TransactionState.COMMITTED);

		} catch (Exception e) {
			this.txResult.setState(TransactionState.ROLLING_BACK);

			try {
				undoCommands();
			} catch (Exception ex) {
				logger.error("Failed to commit transaction and then undo commands due to " + ex.getMessage(), ex);
				try {
					rollback();
					handleRollback(start);
				} catch (Exception exc) {
					logger.error("Failed to roll back after failing to undo commands: " + exc.getMessage(),
							exc); //$NON-NLS-1$
				}
				logger.error("Transaction failed due to " + e.getMessage(), e);
				handleFailure(false, start, ex);
			}

			try {
				rollback();
				handleRollback(start);
			} catch (Exception ex) {
				logger.error("Transaction failed due to " + e.getMessage(), e);
				logger.error("Failed to commit transaction and then rollback due to " + ex.getMessage(), ex);
				handleFailure(false, start, e);
			}

			this.txResult.setState(TransactionState.FAILED);

			handleFailure(true, start, e);

		} finally {
			releaseElementLocks();
		}
	}

	@Override
	public void autoCloseableRollback() {
		long start = System.nanoTime();
		logger.warn(MessageFormat.format("Rolling back TX for realm {0}...", getRealmName())); //$NON-NLS-1$
		try {
			this.txResult.setState(TransactionState.ROLLING_BACK);
			undoCommands();
			rollback();
			handleRollback(start);
			this.txResult.setState(TransactionState.ROLLED_BACK);
		} catch (Exception e) {
			handleFailure(true, start, e);
			this.txResult.setState(TransactionState.FAILED);
		} finally {
			releaseElementLocks();
		}
	}

	@Override
	public void autoCloseableReadOnly() throws StrolchTransactionException {
		long start = System.nanoTime();
		try {
			this.txResult.setState(TransactionState.CLOSING);

			if (!this.commands.isEmpty()) {
				autoCloseableRollback();
				String msg = "There are commands registered on a read-only transaction. Changing to rollback! Did you forget to commit?";
				throw new IllegalStateException(msg);
			}

			if (this.objectFilter != null && !this.objectFilter.isEmpty()) {
				autoCloseableRollback();
				String msg = "There are modified objects registered on a read-only transaction. Changing to rollback! Did you forget to commit?";
				throw new IllegalStateException(msg);
			}

			long auditTrailDuration = writeAuditTrail();

			// rollback and release any resources
			rollback();

			handleReadOnly(start, auditTrailDuration);
			this.txResult.setState(TransactionState.CLOSED);
		} catch (Exception e) {
			this.txResult.setState(TransactionState.ROLLING_BACK);
			try {
				undoCommands();
				rollback();
			} catch (Exception e1) {
				logger.error("Failed to rollback after exception", e);
			}
			handleFailure(true, start, e);
			this.txResult.setState(TransactionState.FAILED);
		} finally {
			releaseElementLocks();
		}
	}

	protected abstract void writeChanges() throws Exception;

	protected abstract void rollback() throws Exception;

	protected abstract void commit() throws Exception;

	private void handleReadOnly(long start, long auditTrailDuration) {

		if (this.suppressDoNothingLogging)
			return;

		long end = System.nanoTime();
		long txDuration = end - this.txResult.getStartNanos();
		long closeDuration = end - start;

		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);

		StringBuilder sb = new StringBuilder();
		sb.append("TX user=");
		sb.append(this.certificate.getUsername());

		sb.append(", realm="); //$NON-NLS-1$
		sb.append(getRealmName());

		sb.append(", took="); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));

		sb.append(", action=");
		sb.append(this.action);

		if (closeDuration >= 100000000L) {
			sb.append(", close="); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(closeDuration));
		}

		if (isAuditTrailEnabled() && auditTrailDuration >= 100000000L) {
			sb.append(", auditTrail="); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(auditTrailDuration));
		}

		logger.info(sb.toString());
	}

	private void handleCommit(long start, long auditTrailDuration, long observerUpdateDuration) {

		long end = System.nanoTime();
		long txDuration = end - this.txResult.getStartNanos();
		long closeDuration = end - start;

		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);

		StringBuilder sb = new StringBuilder();
		sb.append("TX user=");
		sb.append(this.certificate.getUsername());

		sb.append(", realm="); //$NON-NLS-1$
		sb.append(getRealmName());

		sb.append(", took="); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));

		sb.append(", action=");
		sb.append(this.action);

		if (closeDuration >= 100000000L) {
			sb.append(", close="); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(closeDuration));
		}

		if (isAuditTrailEnabled() && auditTrailDuration >= 100000000L) {
			sb.append(", auditTrail="); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(auditTrailDuration));
		}

		if (isObserverUpdatesEnabled() && observerUpdateDuration >= 100000000L) {
			sb.append(", updates="); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(observerUpdateDuration));
		}
		logger.info(sb.toString());
	}

	private void handleRollback(long start) {

		long end = System.nanoTime();
		long txDuration = end - this.txResult.getStartNanos();
		long closeDuration = end - start;

		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);

		StringBuilder sb = new StringBuilder();
		sb.append("TX ROLLBACK user=");
		sb.append(this.certificate.getUsername());

		sb.append(", realm="); //$NON-NLS-1$
		sb.append(getRealmName());

		sb.append(" failed="); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));

		sb.append(", action=");
		sb.append(this.action);

		if (closeDuration >= 100000000L) {
			sb.append(", close="); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(closeDuration));
		}
		logger.error(sb.toString());
	}

	private void handleFailure(boolean throwEx, long closeStartNanos, Exception e) {

		long end = System.nanoTime();
		long txDuration = end - this.txResult.getStartNanos();
		long closeDuration = end - closeStartNanos;

		this.txResult.setState(TransactionState.FAILED);
		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);

		StringBuilder sb = new StringBuilder();
		sb.append("TX FAILED user=");
		sb.append(this.certificate.getUsername());

		sb.append(", realm="); //$NON-NLS-1$
		sb.append(getRealmName());

		sb.append(" failed="); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));

		sb.append(", action=");
		sb.append(this.action);

		if (closeDuration >= 100000000L) {
			sb.append(", close="); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(closeDuration));
		}

		if (this.operationsLog != null) {
			this.operationsLog.addMessage(new LogMessage(this.realm.getRealm(), this.certificate.getUsername(),
					Locator.valueOf(AGENT, "tx", this.action, StrolchAgent.getUniqueId()), LogSeverity.Exception,
					ResourceBundle.getBundle("strolch-agent"), "agent.tx.failed").withException(e).value("reason", e));
		}

		String msg = "Strolch Transaction for realm {0} failed due to {1}\n{2}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, getRealmName(), ExceptionHelper.getExceptionMessage(e), sb.toString());
		StrolchTransactionException ex = new StrolchTransactionException(msg, e);

		if (throwEx)
			throw ex;
		else
			logger.error("Transaction failed in non-commit mode! Logging TX exception, so exception is not suppressed.",
					ex);
	}

	private boolean isAuditTrailEnabled() {
		return getAuditTrail().isEnabled();
	}

	private long updateObservers() {
		if (!isObserverUpdatesEnabled())
			return 0L;

		long observerUpdateStart = System.nanoTime();

		ObserverEvent event = new ObserverEvent();

		if (this.resourceMap != null) {
			if (!this.resourceMap.getCreated().isEmpty())
				event.added.addList(Tags.RESOURCE, new ArrayList<>(this.resourceMap.getCreated()));
			if (!this.resourceMap.getUpdated().isEmpty())
				event.updated.addList(Tags.RESOURCE, new ArrayList<>(this.resourceMap.getUpdated()));
			if (!this.resourceMap.getDeleted().isEmpty())
				event.removed.addList(Tags.RESOURCE, new ArrayList<>(this.resourceMap.getDeleted()));
		}

		if (this.orderMap != null) {
			if (!this.orderMap.getCreated().isEmpty())
				event.added.addList(Tags.ORDER, new ArrayList<>(this.orderMap.getCreated()));
			if (!this.orderMap.getUpdated().isEmpty())
				event.updated.addList(Tags.ORDER, new ArrayList<>(this.orderMap.getUpdated()));
			if (!this.orderMap.getDeleted().isEmpty())
				event.removed.addList(Tags.ORDER, new ArrayList<>(this.orderMap.getDeleted()));
		}

		if (this.activityMap != null) {
			if (!this.activityMap.getCreated().isEmpty())
				event.added.addList(Tags.ACTIVITY, new ArrayList<>(this.activityMap.getCreated()));
			if (!this.activityMap.getUpdated().isEmpty())
				event.updated.addList(Tags.ACTIVITY, new ArrayList<>(this.activityMap.getUpdated()));
			if (!this.activityMap.getDeleted().isEmpty())
				event.removed.addList(Tags.ACTIVITY, new ArrayList<>(this.activityMap.getDeleted()));
		}

		ObserverHandler observerHandler = this.realm.getObserverHandler();
		observerHandler.notify(event);

		return System.nanoTime() - observerUpdateStart;
	}

	private boolean isObserverUpdatesEnabled() {
		return !this.suppressUpdates && this.realm.isUpdateObservers();
	}

	private long writeAuditTrail() {
		if (!isAuditTrailEnabled())
			return 0L;
		if (isSuppressAudits())
			return 0L;

		long auditTrailStart = System.nanoTime();

		List<Audit> audits = new ArrayList<>();

		if (this.orderMap != null) {
			if (this.realm.isAuditTrailEnabledForRead())
				auditsFor(audits, AccessType.READ, this.orderMap.getRead());
			auditsFor(audits, AccessType.CREATE, this.orderMap.getCreated());
			auditsFor(audits, AccessType.UPDATE, this.orderMap.getUpdated());
			auditsFor(audits, AccessType.DELETE, this.orderMap.getDeleted());
		}

		if (this.resourceMap != null) {
			if (this.realm.isAuditTrailEnabledForRead())
				auditsFor(audits, AccessType.READ, this.resourceMap.getRead());
			auditsFor(audits, AccessType.CREATE, this.resourceMap.getCreated());
			auditsFor(audits, AccessType.UPDATE, this.resourceMap.getUpdated());
			auditsFor(audits, AccessType.DELETE, this.resourceMap.getDeleted());
		}

		if (this.activityMap != null) {
			if (this.realm.isAuditTrailEnabledForRead())
				auditsFor(audits, AccessType.READ, this.activityMap.getRead());
			auditsFor(audits, AccessType.CREATE, this.activityMap.getCreated());
			auditsFor(audits, AccessType.UPDATE, this.activityMap.getUpdated());
			auditsFor(audits, AccessType.DELETE, this.activityMap.getDeleted());
		}

		if (this.auditTrail != null && !isSuppressAuditsForAudits()) {
			if (this.realm.isAuditTrailEnabledForRead())
				auditsForAudits(audits, AccessType.READ, Tags.AUDIT, this.auditTrail.getRead());
			auditsForAudits(audits, AccessType.CREATE, Tags.AUDIT, this.auditTrail.getCreated());
			auditsForAudits(audits, AccessType.UPDATE, Tags.AUDIT, this.auditTrail.getUpdated());
			auditsForAudits(audits, AccessType.DELETE, Tags.AUDIT, this.auditTrail.getDeleted());
		}

		if (!audits.isEmpty())
			this.realm.getAuditTrail().addAll(this, audits);

		return System.nanoTime() - auditTrailStart;
	}

	private <T extends StrolchRootElement> void auditsFor(List<Audit> audits, AccessType accessType, Set<T> elements) {
		for (StrolchRootElement element : elements) {
			audits.add(auditFrom(accessType, element));
		}
	}

	private void auditsForAudits(List<Audit> audits, AccessType accessType, String elementType, Set<Audit> elements) {
		for (Audit element : elements) {
			audits.add(auditFrom(accessType, elementType, StringHelper.DASH, element.getId().toString()));
		}
	}

	@Override
	public Audit auditFrom(AccessType accessType, StrolchRootElement element) {
		String type = element.getObjectType();
		String subType = element.getType();
		String id = element.getId();
		return auditFrom(accessType, type, subType, id);
	}

	@Override
	public Audit auditFrom(AccessType accessType, String elementType, String elementSubType, String id) {
		Audit audit = new Audit();

		audit.setId(StrolchAgent.getUniqueIdLong());
		audit.setUsername(this.certificate.getUsername());
		audit.setFirstname(this.certificate.getFirstname() == null ?
				this.certificate.getUsername() :
				this.certificate.getFirstname());
		audit.setLastname(this.certificate.getLastname() == null ?
				this.certificate.getUsername() :
				this.certificate.getLastname());
		audit.setDate(new Date());

		audit.setElementType(elementType);
		audit.setElementSubType(elementSubType);
		audit.setElementAccessed(id);

		// audit.setNewVersion();

		audit.setAction(this.action);
		audit.setAccessType(accessType);

		return audit;
	}

	/**
	 * Calls {@link Command#validate()} on all registered command. This is done before we perform any commands and thus
	 * no rollback needs be done due to invalid input for a command
	 */
	private void validateCommands() {
		for (Command command : this.commands) {
			command.validate();
		}
	}

	/**
	 * Calls {@link Command#doCommand()} on all registered commands. This is done after the commands have been validated
	 * so chance of a runtime exception should be small
	 */
	private void doCommands() {

		// keep track of the current list of commands
		List<Command> commands = this.commands;

		// assign a new list for in case a command adds new commands
		this.commands = new ArrayList<>();

		ListIterator<Command> iter = commands.listIterator();
		while (iter.hasNext()) {
			Command command = iter.next();
			this.flushedCommands.add(command);
			command.doCommand();
			iter.remove();
		}
	}

	/**
	 * Calls {@link Command#undo()} on all registered commands. This is done when an exception is caught while
	 * performing the commands
	 */
	private void undoCommands() {
		ListIterator<Command> iter = this.flushedCommands.listIterator(this.flushedCommands.size());
		while (iter.hasPrevious()) {
			Command command = iter.previous();
			command.undo();
			iter.remove();
		}
	}
}
