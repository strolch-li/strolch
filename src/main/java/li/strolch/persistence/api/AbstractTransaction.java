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

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.ParameterBag;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.inmemory.InMemoryTransaction;
import li.strolch.runtime.observer.ObserverHandler;
import li.strolch.service.api.Command;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

	public AbstractTransaction(StrolchRealm realm) {
		this.realm = realm;
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
	public <U> List<U> doQuery(OrderQuery<U> query) {
		return getPersistenceHandler().getOrderDao(this).doQuery(query);
	}

	@Override
	public <U> List<U> doQuery(ResourceQuery<U> query) {
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

	@Override
	public void autoCloseableCommit() {
		long start = System.nanoTime();
		if (logger.isDebugEnabled()) {
			logger.info("Committing TX for realm " + getRealmName() + "..."); //$NON-NLS-1$
		}

		try {
			this.txResult.setState(TransactionState.COMMITTING);

			validateCommands();
			doCommands();
			commit(this.txResult);

			long observerUpdateStart = System.nanoTime();
			updateObservers();
			long observerUpdateDuration = System.nanoTime() - observerUpdateStart;

			handleCommit(start, observerUpdateDuration);

		} catch (Exception e) {
			this.txResult.setState(TransactionState.ROLLING_BACK);
			try {
				undoCommands();
			} catch (Exception e2) {
				try {
					rollback(txResult);
					handleRollback(start);
				} catch (Exception e1) {
					logger.error("Failed to roll back after failing to undo commands: " + e1.getMessage(), e1);
				}
				handleFailure(start, e);
			}
			try {
				rollback(txResult);
				handleRollback(start);
			} catch (Exception e1) {
				handleFailure(start, e);
			}

			String msg = "Strolch Transaction for realm {0} failed due to {1}";
			msg = MessageFormat.format(msg, getRealmName(), e.getMessage());
			throw new StrolchPersistenceException(msg, e);

		} finally {
			unlockElements();
		}
	}

	@Override
	public void autoCloseableRollback() {
		long start = System.nanoTime();
		logger.warn("Rolling back TX for realm " + getRealmName() + "..."); //$NON-NLS-1$
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

	protected abstract void commit(TransactionResult txResult) throws Exception;

	protected abstract void rollback(TransactionResult txResult) throws Exception;

	private void handleCommit(long start, long observerUpdateDuration) {

		long end = System.nanoTime();
		long txDuration = end - txResult.getStartNanos();
		long closeDuration = end - start;

		this.txResult.setState(TransactionState.COMMITTED);
		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);

		StringBuilder sb = new StringBuilder();
		sb.append("TX for realm ");
		sb.append(getRealmName());
		sb.append(" was completed after "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));
		sb.append(" with close operation taking "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(closeDuration));
		sb.append(" and observer updates took ");
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
		sb.append("TX");
		sb.append(getRealmName());
		sb.append(" has failed after "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));
		sb.append(" with close operation taking "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(closeDuration));
		logger.info(sb.toString());

		String msg = "Strolch Transaction for realm {0} failed due to {1}";
		msg = MessageFormat.format(msg, getRealmName(), e.getMessage());
		throw new StrolchPersistenceException(msg, e);
	}

	private void updateObservers() {
		if (!this.suppressUpdates && this.observerHandler != null) {

			Set<String> keys = this.txResult.getKeys();
			for (String key : keys) {
				ModificationResult modificationResult = this.txResult.getModificationResult(key);

				this.observerHandler.add(key, modificationResult.<StrolchElement> getCreated());
				this.observerHandler.update(key, modificationResult.<StrolchElement> getUpdated());
				this.observerHandler.remove(key, modificationResult.<StrolchElement> getDeleted());
			}
		}
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
