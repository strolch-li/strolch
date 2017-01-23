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
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ObserverHandler;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchLockException;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.agent.impl.AuditingActivityMap;
import li.strolch.agent.impl.AuditingAuditMapFacade;
import li.strolch.agent.impl.AuditingOrderMap;
import li.strolch.agent.impl.AuditingResourceMap;
import li.strolch.agent.impl.InternalStrolchRealm;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchQuery;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.visitor.ElementTypeVisitor;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractTransaction implements StrolchTransaction {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractTransaction.class);
	private InternalStrolchRealm realm;

	private TransactionCloseStrategy closeStrategy;
	private boolean suppressUpdates;
	private boolean suppressAudits;
	private boolean suppressDoNothingLogging;
	private TransactionResult txResult;

	private List<Command> commands;
	private List<Command> flushedCommands;
	private Set<StrolchRootElement> lockedElements;

	private AuditingOrderMap orderMap;
	private AuditingResourceMap resourceMap;
	private AuditingActivityMap activityMap;
	private AuditingAuditMapFacade auditTrail;

	private String action;
	private Certificate certificate;
	private PrivilegeHandler privilegeHandler;

	public AbstractTransaction(PrivilegeHandler privilegeHandler, StrolchRealm realm, Certificate certificate,
			String action) {
		DBC.PRE.assertNotNull("privilegeHandler must be set!", privilegeHandler); //$NON-NLS-1$
		DBC.PRE.assertNotNull("realm must be set!", realm); //$NON-NLS-1$
		DBC.PRE.assertNotNull("certificate must be set!", certificate); //$NON-NLS-1$
		DBC.PRE.assertNotNull("action must be set!", action); //$NON-NLS-1$

		this.privilegeHandler = privilegeHandler;
		this.realm = (InternalStrolchRealm) realm;
		this.action = action;
		this.certificate = certificate;

		this.commands = new ArrayList<>();
		this.flushedCommands = new ArrayList<>();
		this.lockedElements = new HashSet<>();
		this.closeStrategy = TransactionCloseStrategy.DO_NOTHING;
		this.txResult = new TransactionResult(getRealmName(), System.nanoTime(), new Date());
		this.txResult.setState(TransactionState.OPEN);
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
		setCloseStrategy(TransactionCloseStrategy.DO_NOTHING);
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
	public boolean isSuppressAudits() {
		return this.suppressAudits;
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
	public <T extends StrolchRootElement> void lock(T element) throws StrolchLockException {
		this.realm.lock(element);
		this.lockedElements.add(element);
	}

	@Override
	public <T extends StrolchRootElement> void releaseLock(T element) throws StrolchLockException {
		this.realm.releaseLock(element);
		this.lockedElements.remove(element);
	}

	private void releaseElementLocks() {
		for (StrolchRootElement lockedElement : this.lockedElements) {
			this.realm.releaseLock(lockedElement);
		}
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

	private void assertQueryAllowed(StrolchQuery query) {
		try {
			PrivilegeContext privilegeContext = this.privilegeHandler.getPrivilegeContext(this.certificate);
			privilegeContext.validateAction(query);
		} catch (PrivilegeException e) {
			throw new StrolchAccessDeniedException(this.certificate, query, ExceptionHelper.getExceptionMessage(e), e);
		}
	}

	@Override
	public <U> List<U> doQuery(OrderQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("orderVisitor", query.getOrderVisitor());
		return getOrderMap().doQuery(this, query);
	}

	@Override
	public <U> List<U> doQuery(ResourceQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("resourceVisitor", query.getResourceVisitor());
		return getResourceMap().doQuery(this, query);
	}

	@Override
	public <U> List<U> doQuery(ActivityQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("activityVisitor", query.getActivityVisitor());
		return getActivityMap().doQuery(this, query);
	}

	@Override
	public <U> List<U> doQuery(AuditQuery<U> query) {
		assertQueryAllowed(query);
		DBC.PRE.assertNotNull("auditVisitor", query.getAuditVisitor());
		return getAuditTrail().doQuery(this, query);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends StrolchElement> T findElement(Locator locator) {

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
			groupedParameterizedElement = getResourceMap().getBy(this, type, id);
			break;
		case Tags.ORDER:
			groupedParameterizedElement = getOrderMap().getBy(this, type, id);
			break;
		case Tags.ACTIVITY:
			groupedParameterizedElement = getActivityMap().getBy(this, type, id);
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

		// state or bag
		String stateOrBagOrActivity = elements.get(3);
		if (stateOrBagOrActivity.equals(Tags.BAG)) {

			String parameterBagId = elements.get(4);
			ParameterBag bag = groupedParameterizedElement.getParameterBag(parameterBagId);
			if (bag == null) {
				String msg = "Could not find ParameterBag for locator {0} on element {1}"; //$NON-NLS-1$
				throw new StrolchException(
						MessageFormat.format(msg, locator, groupedParameterizedElement.getLocator()));
			}

			if (elements.size() == 5)
				return (T) bag;

			String parameterId = elements.get(5);
			Parameter<?> parameter = bag.getParameter(parameterId);
			return (T) parameter;

		} else if (stateOrBagOrActivity.equals(Tags.STATE)) {

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

		String msg = "Invalid locator {0} with part {1}"; //$NON-NLS-1$
		throw new StrolchException(MessageFormat.format(msg, locator, stateOrBagOrActivity));
	}

	@Override
	public Resource getResourceTemplate(String type) {
		return getResourceMap().getTemplate(this, type);
	}

	@Override
	public Resource getResourceTemplate(String type, boolean assertExists) throws StrolchException {
		return getResourceMap().getTemplate(this, type, assertExists);
	}

	@Override
	public Order getOrderTemplate(String type) {
		return getOrderMap().getTemplate(this, type);
	}

	@Override
	public Order getOrderTemplate(String type, boolean assertExists) throws StrolchException {
		return getOrderMap().getTemplate(this, type, assertExists);
	}

	@Override
	public Activity getActivityTemplate(String type) {
		return getActivityMap().getTemplate(this, type);
	}

	@Override
	public Activity getActivityTemplate(String type, boolean assertExists) throws StrolchException {
		return getActivityMap().getTemplate(this, type, assertExists);
	}

	@Override
	public Order getOrderBy(String type, String id) {
		return getOrderBy(type, id, false);
	}

	@Override
	public Order getOrderBy(String type, String id, boolean assertExists) throws StrolchException {
		return getOrderMap().getBy(this, type, id, assertExists);
	}

	@Override
	public Order getOrderBy(StringParameter refP) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getOrderBy(refP, false);
	}

	@Override
	public Order getOrderBy(StringParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getOrderMap().getBy(this, refP, assertExists);
	}

	@Override
	public List<Order> getOrdersBy(StringListParameter refP) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getOrderMap().getBy(this, refP, false);
	}

	@Override
	public List<Order> getOrdersBy(StringListParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getOrderMap().getBy(this, refP, assertExists);
	}

	@Override
	public Resource getResourceBy(String type, String id) {
		return getResourceBy(type, id, false);
	}

	@Override
	public Resource getResourceBy(String type, String id, boolean assertExists) throws StrolchException {
		Resource resource = getResourceMap().getBy(this, type, id);
		if (assertExists && resource == null) {
			String msg = "No Resource exists with the id {0} with type {1}";
			throw new StrolchException(MessageFormat.format(msg, id, type));
		}
		return resource;
	}

	@Override
	public Resource getResourceBy(StringParameter refP) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getResourceBy(refP, false);
	}

	@Override
	public Resource getResourceBy(StringParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getResourceMap().getBy(this, refP, assertExists);
	}

	@Override
	public List<Resource> getResourcesBy(StringListParameter refP) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getResourceMap().getBy(this, refP, false);
	}

	@Override
	public List<Resource> getResourcesBy(StringListParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getResourceMap().getBy(this, refP, assertExists);
	}

	@Override
	public Activity getActivityBy(String type, String id) {
		return getActivityBy(type, id, false);
	}

	@Override
	public Activity getActivityBy(String type, String id, boolean assertExists) throws StrolchException {
		Activity activity = getActivityMap().getBy(this, type, id);
		if (assertExists && activity == null) {
			String msg = "No Activity exists with the id {0} with type {1}";
			throw new StrolchException(MessageFormat.format(msg, id, type));
		}
		return activity;
	}

	@Override
	public Activity getActivityBy(StringParameter refP) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getActivityBy(refP, false);
	}

	@Override
	public Activity getActivityBy(StringParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getActivityMap().getBy(this, refP, assertExists);
	}

	@Override
	public List<Activity> getActivitiesBy(StringListParameter refP) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getActivityMap().getBy(this, refP, false);
	}

	@Override
	public List<Activity> getActivitiesBy(StringListParameter refP, boolean assertExists) throws StrolchException {
		DBC.PRE.assertNotNull("refP", refP);
		return getActivityMap().getBy(this, refP, assertExists);
	}

	@Override
	public void flush() {
		try {
			validateCommands();
			doCommands();
			writeChanges();
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

			validateCommands();
			doCommands();
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
					logger.error("Failed to roll back after failing to undo commands: " + exc.getMessage(), exc); //$NON-NLS-1$
				}
				logger.error("Transaction failed due to " + e.getMessage(), e);
				handleFailure(start, ex);
			}

			try {
				rollback();
				handleRollback(start);
			} catch (Exception ex) {
				logger.error("Transaction failed due to " + e.getMessage(), e);
				logger.error("Failed to commit transaction and then rollback due to " + ex.getMessage(), ex);
				handleFailure(start, e);
			}

			this.txResult.setState(TransactionState.FAILED);

			String msg = "Strolch Transaction for realm {0} failed due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getRealmName(), ExceptionHelper.getExceptionMessage(e));
			throw new StrolchTransactionException(msg, e);

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
			handleFailure(start, e);
			this.txResult.setState(TransactionState.FAILED);
		} finally {
			releaseElementLocks();
		}
	}

	@Override
	public void autoCloseableDoNothing() throws StrolchTransactionException {
		long start = System.nanoTime();
		try {
			this.txResult.setState(TransactionState.CLOSING);

			// TODO re-think this.
			if (!this.commands.isEmpty()) {
				logger.error(
						"There are commands registered on a read-only transaction. Changing to rollback! Probably due to an exception!");
				autoCloseableRollback();
				return;
			}

			long auditTrailDuration = writeAuditTrail();

			// rollback and release any resources
			rollback();

			handleDoNothing(start, auditTrailDuration);
			this.txResult.setState(TransactionState.CLOSED);
		} catch (Exception e) {
			handleFailure(start, e);
			this.txResult.setState(TransactionState.FAILED);
		} finally {
			releaseElementLocks();
		}
	}

	protected abstract void writeChanges() throws Exception;

	protected abstract void rollback() throws Exception;

	protected abstract void commit() throws Exception;

	private void handleDoNothing(long start, long auditTrailDuration) {

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

	protected void handleFailure(long closeStartNanos, Exception e) {

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

		String msg = "Strolch Transaction for realm {0} failed due to {1}\n{2}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, getRealmName(), ExceptionHelper.getExceptionMessage(e), sb.toString());
		throw new StrolchTransactionException(msg, e);
	}

	private boolean isAuditTrailEnabled() {
		return getAuditTrail().isEnabled();
	}

	private long updateObservers() {
		if (!isObserverUpdatesEnabled())
			return 0L;

		long observerUpdateStart = System.nanoTime();

		ObserverHandler observerHandler = this.realm.getObserverHandler();

		if (this.orderMap != null) {
			observerHandler.add(Tags.ORDER, new ArrayList<StrolchRootElement>(this.orderMap.getCreated()));
			observerHandler.update(Tags.ORDER, new ArrayList<StrolchRootElement>(this.orderMap.getUpdated()));
			observerHandler.remove(Tags.ORDER, new ArrayList<StrolchRootElement>(this.orderMap.getDeleted()));
		}

		if (this.resourceMap != null) {
			observerHandler.add(Tags.RESOURCE, new ArrayList<StrolchRootElement>(this.resourceMap.getCreated()));
			observerHandler.update(Tags.RESOURCE, new ArrayList<StrolchRootElement>(this.resourceMap.getUpdated()));
			observerHandler.remove(Tags.RESOURCE, new ArrayList<StrolchRootElement>(this.resourceMap.getDeleted()));
		}

		long observerUpdateDuration = System.nanoTime() - observerUpdateStart;
		return observerUpdateDuration;
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

		// this is bad... doesn't account for a created and deleted in same TX...

		if (this.orderMap != null) {
			if (this.realm.isAuditTrailEnabledForRead())
				auditsFor(audits, AccessType.READ, Tags.ORDER, this.orderMap.getRead());
			auditsFor(audits, AccessType.CREATE, Tags.ORDER, this.orderMap.getCreated());
			auditsFor(audits, AccessType.UPDATE, Tags.ORDER, this.orderMap.getUpdated());
			auditsFor(audits, AccessType.DELETE, Tags.ORDER, this.orderMap.getDeleted());
		}

		if (this.resourceMap != null) {
			if (this.realm.isAuditTrailEnabledForRead())
				auditsFor(audits, AccessType.READ, Tags.RESOURCE, this.resourceMap.getRead());
			auditsFor(audits, AccessType.CREATE, Tags.RESOURCE, this.resourceMap.getCreated());
			auditsFor(audits, AccessType.UPDATE, Tags.RESOURCE, this.resourceMap.getUpdated());
			auditsFor(audits, AccessType.DELETE, Tags.RESOURCE, this.resourceMap.getDeleted());
		}

		if (this.auditTrail != null) {
			if (this.realm.isAuditTrailEnabledForRead())
				auditsForAudits(audits, AccessType.READ, Tags.AUDIT, this.auditTrail.getRead());
			auditsForAudits(audits, AccessType.CREATE, Tags.AUDIT, this.auditTrail.getCreated());
			auditsForAudits(audits, AccessType.UPDATE, Tags.AUDIT, this.auditTrail.getUpdated());
			auditsForAudits(audits, AccessType.DELETE, Tags.AUDIT, this.auditTrail.getDeleted());
		}

		this.realm.getAuditTrail().addAll(this, audits);
		long auditTrailDuration = System.nanoTime() - auditTrailStart;
		return auditTrailDuration;
	}

	private <T extends StrolchRootElement> void auditsFor(List<Audit> audits, AccessType accessType, String elementType,
			Set<T> elements) {
		for (StrolchRootElement element : elements) {
			audits.add(auditFrom(accessType, element));
		}
	}

	private <T extends StrolchRootElement> void auditsForAudits(List<Audit> audits, AccessType accessType,
			String elementType, Set<Audit> elements) {
		for (Audit element : elements) {
			audits.add(auditFrom(accessType, elementType, StringHelper.DASH, element.getId().toString()));
		}
	}

	@Override
	public Audit auditFrom(AccessType accessType, StrolchRootElement element) {
		String type = element.accept(new ElementTypeVisitor());
		String subType = element.getType();
		String id = element.getId();
		return auditFrom(accessType, type, subType, id);
	}

	@Override
	public Audit auditFrom(AccessType accessType, String elementType, String elementSubType, String id) {
		Audit audit = new Audit();

		audit.setId(StrolchAgent.getUniqueIdLong());
		audit.setUsername(this.certificate.getUsername());
		audit.setFirstname(this.certificate.getFirstname());
		audit.setLastname(this.certificate.getLastname());
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
		ListIterator<Command> iter = this.commands.listIterator();
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
