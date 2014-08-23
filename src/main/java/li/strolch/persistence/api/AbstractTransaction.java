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
import java.util.List;
import java.util.Set;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.OrderVisitor;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.model.audit.NoStrategyAuditVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.visitor.ElementTypeVisitor;
import li.strolch.model.visitor.NoStrategyOrderVisitor;
import li.strolch.model.visitor.NoStrategyResourceVisitor;
import li.strolch.persistence.inmemory.InMemoryTransaction;
import li.strolch.runtime.observer.ObserverHandler;
import li.strolch.service.api.Command;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractTransaction implements StrolchTransaction {

	protected static final Logger logger = LoggerFactory.getLogger(InMemoryTransaction.class);
	private StrolchRealm realm;

	private TransactionCloseStrategy closeStrategy;
	private ObserverHandler observerHandler;
	private boolean suppressUpdates;
	private TransactionResult txResult;

	private List<Command> commands;
	private Set<StrolchRootElement> lockedElements;

	private String action;
	private Certificate certificate;

	public AbstractTransaction(StrolchRealm realm, Certificate certificate, String action) {
		DBC.PRE.assertNotNull("realm must be set!", realm);
		DBC.PRE.assertNotNull("certificate must be set!", certificate);
		DBC.PRE.assertNotNull("action must be set!", action);

		this.realm = realm;
		this.action = action;
		this.certificate = certificate;

		this.commands = new ArrayList<>();
		this.lockedElements = new HashSet<>();
		this.closeStrategy = TransactionCloseStrategy.COMMIT;
		this.txResult = new TransactionResult(getRealmName(), System.nanoTime(), new Date());
		this.txResult.setState(TransactionState.OPEN);
	}

	@Override
	public boolean isOpen() {
		return this.txResult.getState() == TransactionState.OPEN;
	}

	@Override
	public boolean isRollingBack() {
		return this.txResult.getState() == TransactionState.ROLLING_BACK;
	}

	@Override
	public boolean isCommitting() {
		return this.txResult.getState() == TransactionState.COMMITTING;
	}

	@Override
	public boolean isCommitted() {
		return this.txResult.getState() == TransactionState.COMMITTED;
	}

	@Override
	public boolean isRolledBack() {
		return this.txResult.getState() == TransactionState.ROLLED_BACK;
	}

	@Override
	public String getRealmName() {
		return this.realm.getRealm();
	}

	protected StrolchRealm getRealm() {
		return this.realm;
	}

	@Override
	public void setCloseStrategy(TransactionCloseStrategy closeStrategy) {
		this.closeStrategy = closeStrategy;
	}

	@Override
	public void close() throws StrolchPersistenceException {
		this.closeStrategy.close(this);
	}

	/**
	 * @param suppressUpdates
	 *            the suppressUpdates to set
	 */
	public void setSuppressUpdates(boolean suppressUpdates) {
		this.suppressUpdates = suppressUpdates;
	}

	/**
	 * @return the suppressUpdates
	 */
	public boolean isSuppressUpdates() {
		return this.suppressUpdates;
	}

	/**
	 * @param observerHandler
	 *            the observerHandler to set
	 */
	public void setObserverHandler(ObserverHandler observerHandler) {
		this.observerHandler = observerHandler;
	}

	@Override
	public <T extends StrolchRootElement> void lock(T element) {
		this.realm.lock(element);
		this.lockedElements.add(element);
	}

	private void unlockElements() {
		for (StrolchRootElement lockedElement : this.lockedElements) {
			this.realm.unlock(lockedElement);
		}
	}

	@Override
	public void addCommand(Command command) {
		this.commands.add(command);
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
	public AuditTrail getAuditTrail() {
		return this.realm.getAuditTrail();
	}

	@Override
	public List<Order> doQuery(OrderQuery query) {
		return getPersistenceHandler().getOrderDao(this).doQuery(query, new NoStrategyOrderVisitor());
	}

	@Override
	public <U> List<U> doQuery(OrderQuery query, OrderVisitor<U> orderVisitor) {
		return getPersistenceHandler().getOrderDao(this).doQuery(query, orderVisitor);
	}

	@Override
	public List<Resource> doQuery(ResourceQuery query) {
		return getPersistenceHandler().getResourceDao(this).doQuery(query, new NoStrategyResourceVisitor());
	}

	@Override
	public <U> List<U> doQuery(ResourceQuery query, ResourceVisitor<U> resourceVisitor) {
		return getPersistenceHandler().getResourceDao(this).doQuery(query, resourceVisitor);
	}

	@Override
	public List<Audit> doQuery(AuditQuery query) {
		return getPersistenceHandler().getAuditDao(this).doQuery(query, new NoStrategyAuditVisitor());
	}

	@Override
	public <U> List<U> doQuery(AuditQuery query, AuditVisitor<U> auditVisitor) {
		return getPersistenceHandler().getAuditDao(this).doQuery(query, auditVisitor);
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
		String stateOrBag = elements.get(3);
		if (stateOrBag.equals(Tags.BAG)) {

			String parameterBagId = elements.get(4);
			ParameterBag bag = groupedParameterizedElement.getParameterBag(parameterBagId);
			if (bag == null) {
				String msg = "Could not find ParameterBag for locator {0} on element {1}"; //$NON-NLS-1$
				throw new StrolchException(MessageFormat.format(msg, locator, groupedParameterizedElement.getLocator()));
			}

			if (elements.size() == 5)
				return (T) bag;

			String parameterId = elements.get(5);
			Parameter<?> parameter = bag.getParameter(parameterId);
			return (T) parameter;

		} else if (stateOrBag.equals(Tags.STATE)) {

			if (elements.size() != 5) {
				String msg = "Missing state Id on locator {0}"; //$NON-NLS-1$
				throw new StrolchException(MessageFormat.format(msg, locator));
			}

			Resource resource = (Resource) groupedParameterizedElement;
			String stateId = elements.get(4);

			StrolchTimedState<IValue<?>> timedState = resource.getTimedState(stateId);
			return (T) timedState;
		}

		String msg = "Invalid locator {0} on with part {1}"; //$NON-NLS-1$
		throw new StrolchException(MessageFormat.format(msg, locator, stateOrBag));
	}

	@Override
	public Order getOrderBy(StringParameter refP) throws StrolchException {
		return getOrderMap().getBy(this, refP);
	}

	@Override
	public Resource getResourceBy(StringParameter refP) throws StrolchException {
		return getResourceMap().getBy(this, refP);
	}

	@Override
	public void autoCloseableCommit() {
		long start = System.nanoTime();
		if (logger.isDebugEnabled()) {
			logger.info(MessageFormat.format("Committing TX for realm {0}...", getRealmName())); //$NON-NLS-1$
		}

		try {
			this.txResult.setState(TransactionState.COMMITTING);

			validateCommands();
			doCommands();
			writeChanges(this.txResult);

			long auditTrailDuration = writeAuditTrail();
			long updateObserversDuration = updateObservers();

			// commit and close the connection
			commit();

			handleCommit(start, auditTrailDuration, updateObserversDuration);

		} catch (Exception e) {
			this.txResult.setState(TransactionState.ROLLING_BACK);
			try {
				undoCommands();
			} catch (Exception e2) {
				try {
					rollback(this.txResult);
					handleRollback(start);
				} catch (Exception e1) {
					logger.error("Failed to roll back after failing to undo commands: " + e1.getMessage(), e1); //$NON-NLS-1$
				}
				handleFailure(start, e);
			}
			try {
				rollback(this.txResult);
				handleRollback(start);
			} catch (Exception e1) {
				handleFailure(start, e);
			}

			String msg = "Strolch Transaction for realm {0} failed due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getRealmName(), e.getMessage());
			throw new StrolchPersistenceException(msg, e);

		} finally {
			try {

			} catch (Exception e) {
				logger.error("Failed to close connection: " + e.getMessage(), e); //$NON-NLS-1$
			}
			unlockElements();
		}
	}

	@Override
	public void autoCloseableRollback() {
		long start = System.nanoTime();
		logger.warn(MessageFormat.format("Rolling back TX for realm {0}...", getRealmName())); //$NON-NLS-1$
		try {
			undoCommands();
			rollback(this.txResult);
			handleRollback(start);
		} catch (Exception e) {
			handleFailure(start, e);
		} finally {
			unlockElements();
		}
	}

	protected abstract void writeChanges(TransactionResult txResult) throws Exception;

	protected abstract void rollback(TransactionResult txResult) throws Exception;

	protected abstract void commit() throws Exception;

	private void handleCommit(long start, long auditTrailDuration, long observerUpdateDuration) {

		long end = System.nanoTime();
		long txDuration = end - this.txResult.getStartNanos();
		long closeDuration = end - start;

		this.txResult.setState(TransactionState.COMMITTED);
		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);

		StringBuilder sb = new StringBuilder();
		sb.append("TX for realm "); //$NON-NLS-1$
		sb.append(getRealmName());
		sb.append(" took "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));
		sb.append(" close took "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(closeDuration));
		if (isAuditTrailEnabled()) {
			sb.append(" auditTrail took "); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(auditTrailDuration));
		}
		if (observerUpdateDuration > 0L)
			sb.append(" updates took "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(observerUpdateDuration));
		logger.info(sb.toString());
	}

	private void handleRollback(long start) {

		long end = System.nanoTime();
		long txDuration = end - this.txResult.getStartNanos();
		long closeDuration = end - start;

		this.txResult.setState(TransactionState.ROLLED_BACK);
		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);
	}

	protected void handleFailure(long closeStartNanos, Exception e) {

		long end = System.nanoTime();
		long txDuration = end - this.txResult.getStartNanos();
		long closeDuration = end - closeStartNanos;

		this.txResult.setState(TransactionState.FAILED);
		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);

		StringBuilder sb = new StringBuilder();
		sb.append("TX"); //$NON-NLS-1$
		sb.append(getRealmName());
		sb.append(" failed took "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));
		sb.append(" close took "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(closeDuration));
		logger.info(sb.toString());

		String msg = "Strolch Transaction for realm {0} failed due to {1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, getRealmName(), e.getMessage());
		throw new StrolchPersistenceException(msg, e);
	}

	private boolean isAuditTrailEnabled() {
		return getAuditTrail().isEnabled();
	}

	private long updateObservers() {
		if (isObserverUpdatesEnabled())
			return 0L;

		long observerUpdateStart = System.nanoTime();
		Set<String> keys = this.txResult.getKeys();
		for (String key : keys) {
			ModificationResult modificationResult = this.txResult.getModificationResult(key);

			this.observerHandler.add(key, modificationResult.<StrolchElement> getCreated());
			this.observerHandler.update(key, modificationResult.<StrolchElement> getUpdated());
			this.observerHandler.remove(key, modificationResult.<StrolchElement> getDeleted());
		}
		long observerUpdateDuration = System.nanoTime() - observerUpdateStart;
		return observerUpdateDuration;
	}

	private boolean isObserverUpdatesEnabled() {
		return this.suppressUpdates || this.observerHandler == null;
	}

	private long writeAuditTrail() {
		if (!isAuditTrailEnabled())
			return 0L;

		Set<String> keys = this.txResult.getKeys();
		if (keys.isEmpty())
			return 0L;

		long auditTrailStart = System.nanoTime();

		List<Audit> audits = new ArrayList<>();
		for (String key : keys) {
			ModificationResult modificationResult = this.txResult.getModificationResult(key);

			List<StrolchElement> created = modificationResult.getCreated();
			for (StrolchElement strolchElement : created) {
				audits.add(auditFrom(AccessType.CREATE, (StrolchRootElement) strolchElement));
			}

			List<StrolchElement> updated = modificationResult.getUpdated();
			for (StrolchElement strolchElement : updated) {
				audits.add(auditFrom(AccessType.UPDATE, (StrolchRootElement) strolchElement));
			}

			List<StrolchElement> deleted = modificationResult.getDeleted();
			for (StrolchElement strolchElement : deleted) {
				audits.add(auditFrom(AccessType.DELETE, (StrolchRootElement) strolchElement));
			}
		}

		getAuditTrail().addAll(this, audits);
		long auditTrailDuration = System.nanoTime() - auditTrailStart;
		return auditTrailDuration;
	}

	private Audit auditFrom(AccessType accessType, StrolchRootElement element) {
		Audit audit = new Audit();

		audit.setId(StrolchAgent.getUniqueIdLong());
		audit.setUsername(this.certificate.getUsername());
		audit.setFirstname(this.certificate.getFirstname());
		audit.setLastname(this.certificate.getLastname());
		audit.setDate(new Date());

		audit.setElementType(element.accept(new ElementTypeVisitor()));
		audit.setElementAccessed(element.getId());

		//audit.setNewVersion();

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
		for (Command command : this.commands) {
			command.doCommand();
		}
	}

	/**
	 * Calls {@link Command#undo()} on all registered commands. This is done when an exception is caught while
	 * performing the commands
	 */
	private void undoCommands() {
		for (Command command : this.commands) {
			command.undo();
		}
	}
}
