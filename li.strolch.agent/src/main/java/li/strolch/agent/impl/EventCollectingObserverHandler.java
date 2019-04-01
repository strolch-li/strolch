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

import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

import java.text.MessageFormat;
import java.util.List;
import java.util.ResourceBundle;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.*;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.MapOfLists;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A simple {@link ObserverHandler} which keeps a reference to all registered {@link Observer Observers} and notifies
 * them when one of the notify methods are called
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EventCollectingObserverHandler implements ObserverHandler {

	private static final Logger logger = LoggerFactory.getLogger(DefaultObserverHandler.class);

	private MapOfLists<String, Observer> observerMap;

	private ObserverEvent observerEvent;

	private ScheduledFuture<?> future;
	private StrolchAgent agent;
	private final StrolchRealm realm;

	public EventCollectingObserverHandler(StrolchAgent agent, StrolchRealm realm) {
		this.agent = agent;
		this.realm = realm;
		this.observerMap = new MapOfLists<>();
	}

	@Override
	public void start() {
		// nothing to do
	}

	@Override
	public void stop() {

		if (this.future != null) {
			this.future.cancel(false);
			this.future = null;
		}
	}

	private ScheduledExecutorService getExecutor() {
		return this.agent.getScheduledExecutor("Observer");
	}

	@Override
	public synchronized void notify(ObserverEvent event) {
		if (event.added.isEmpty() && event.updated.isEmpty() && event.removed.isEmpty())
			return;

		synchronized (this) {
			if (this.observerEvent == null) {
				this.observerEvent = event;
			} else {
				this.observerEvent.added.addAll(event.added);
				this.observerEvent.updated.addAll(event.updated);
				this.observerEvent.removed.addAll(event.removed);
			}
		}

		if (this.future == null || this.future.isDone()) {
			this.future = getExecutor().scheduleAtFixedRate(this::doUpdates, 100, 100, TimeUnit.MILLISECONDS);
		}
	}

	protected void doUpdates() {

		ObserverEvent event;
		synchronized (this) {
			if (this.observerEvent == null)
				return;

			event = this.observerEvent;
			this.observerEvent = null;
		}

		synchronized (this.observerMap) {
			for (String key : event.added.keySet()) {
				add(key, event.added.getList(key));
			}
			for (String key : event.updated.keySet()) {
				update(key, event.updated.getList(key));
			}
			for (String key : event.removed.keySet()) {
				remove(key, event.removed.getList(key));
			}
		}

		synchronized (this) {
			if (this.observerEvent != null) {
				this.future = getExecutor().scheduleAtFixedRate(this::doUpdates, 100, 100, TimeUnit.MILLISECONDS);
			}
		}
	}

	private void add(String key, List<StrolchRootElement> elements) {
		if (elements == null || elements.isEmpty())
			return;

		List<Observer> observerList = this.observerMap.getList(key);
		if (observerList != null && !observerList.isEmpty()) {
			for (Observer observer : observerList) {
				try {
					observer.add(key, elements);
				} catch (Exception e) {
					String msg = "Failed to update observer {0} with {1} due to {2}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, key, observer, e.getMessage());
					logger.error(msg, e);

					addLogMessage("add", e);
				}
			}
		}
	}

	private void update(String key, List<StrolchRootElement> elements) {
		if (elements == null || elements.isEmpty())
			return;

		List<Observer> observerList = this.observerMap.getList(key);
		if (observerList != null && !observerList.isEmpty()) {
			for (Observer observer : observerList) {
				try {
					observer.update(key, elements);
				} catch (Exception e) {
					String msg = "Failed to update observer {0} with {1} due to {2}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, key, observer, e.getMessage());
					logger.error(msg, e);

					addLogMessage("update", e);
				}
			}
		}
	}

	private void remove(String key, List<StrolchRootElement> elements) {
		if (elements == null || elements.isEmpty())
			return;

		List<Observer> observerList = this.observerMap.getList(key);
		if (observerList != null && !observerList.isEmpty()) {
			for (Observer observer : observerList) {
				try {
					observer.remove(key, elements);
				} catch (Exception e) {
					String msg = "Failed to update observer {0} with {1} due to {2}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, key, observer, e.getMessage());
					logger.error(msg, e);

					addLogMessage("remove", e);
				}
			}
		}
	}

	private void addLogMessage(String type, Exception e) {
		ComponentContainer container = this.agent.getContainer();
		if (container.hasComponent(OperationsLog.class)) {
			OperationsLog operationsLog = container.getComponent(OperationsLog.class);
			operationsLog.addMessage(new LogMessage(this.realm.getRealm(), SYSTEM_USER_AGENT,
					Locator.valueOf(AGENT, ObserverHandler.class.getName(), type, StrolchAgent.getUniqueId()),
					LogSeverity.Exception, ResourceBundle.getBundle("strolch-agent"), "agent.observers.update.failed")
					.withException(e).value("type", type).value("reason", e));
		}
	}

	@Override
	public void registerObserver(String key, Observer observer) {
		synchronized (this.observerMap) {
			this.observerMap.addElement(key, observer);
			String msg = MessageFormat.format("Registered observer {0} with {1}", key, observer); //$NON-NLS-1$
			logger.info(msg);
		}
	}

	@Override
	public void unregisterObserver(String key, Observer observer) {
		synchronized (this.observerMap) {
			if (this.observerMap.removeElement(key, observer)) {
				String msg = MessageFormat.format("Unregistered observer {0} with {1}", key, observer); //$NON-NLS-1$
				logger.info(msg);
			}
		}
	}
}
