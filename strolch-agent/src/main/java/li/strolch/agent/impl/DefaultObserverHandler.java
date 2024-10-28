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

import li.strolch.agent.api.*;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.utils.collections.MapOfLists;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingDeque;

import static java.util.Optional.ofNullable;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfLists;
import static li.strolch.utils.helper.StringHelper.formatNanoDuration;

/**
 * A simple {@link ObserverHandler} which keeps a reference to all registered {@link Observer Observers} and notifies
 * them when one of the notify methods are called
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultObserverHandler implements ObserverHandler {

	private static final Logger logger = LoggerFactory.getLogger(DefaultObserverHandler.class);
	private final StrolchAgent agent;
	private final StrolchRealm realm;

	private volatile boolean run;

	private final BlockingDeque<ObserverEvent> eventQueue;

	private final MapOfLists<String, Observer> observerMap;
	private Future<?> updateTask;

	public DefaultObserverHandler(StrolchAgent agent, StrolchRealm realm) {
		this.agent = agent;
		this.realm = realm;
		this.observerMap = synchronizedMapOfLists(new MapOfLists<>());
		this.eventQueue = new LinkedBlockingDeque<>();
	}

	@Override
	public void start() {
		this.run = true;
		this.updateTask = this.agent.getSingleThreadExecutor("Observer").submit(this::doUpdates);
	}

	@Override
	public void stop() {
		this.run = false;
		if (this.updateTask != null)
			this.updateTask.cancel(true);
	}

	@Override
	public void notify(ObserverEvent event) {
		assertReadOnly(event);
		if (!(event.added.isEmpty() && event.updated.isEmpty() && event.removed.isEmpty()))
			this.eventQueue.addLast(event);
	}

	private static void assertReadOnly(ObserverEvent event) {
		event.added.values().forEach(DefaultObserverHandler::assertReadOnly);
		event.updated.values().forEach(DefaultObserverHandler::assertReadOnly);
		event.removed.values().forEach(DefaultObserverHandler::assertReadOnly);
	}

	private static void assertReadOnly(StrolchRootElement element) {
		if (!element.isReadOnly())
			throw new IllegalStateException(
					"Only allow to update elements which are read-only. Element %s is not read-only!".formatted(
							element.getLocator()));
	}

	protected void doUpdates() {
		while (this.run) {
			try {
				ObserverEvent event = this.eventQueue.takeFirst();
				long start = System.currentTimeMillis();

				MapOfLists<String, StrolchRootElement> added = event.added;
				MapOfLists<String, StrolchRootElement> updated = event.updated;
				MapOfLists<String, StrolchRootElement> removed = event.removed;
				added.keySet().forEach(k1 -> ofNullable(added.getList(k1)).ifPresent(l1 -> notifyAdd(k1, l1)));
				updated.keySet().forEach(k -> ofNullable(updated.getList(k)).ifPresent(l -> notifyUpdate(k, l)));
				removed.keySet().forEach(k -> ofNullable(removed.getList(k)).ifPresent(l -> notifyRemove(k, l)));

				long durationMs = System.currentTimeMillis() - start;
				if (durationMs >= 500L)
					logger.warn("Observer update for event {} took {}", event, formatNanoDuration(durationMs));

			} catch (InterruptedException e) {
				if (this.run)
					logger.error("Failed to do updates!", e);
				else
					logger.warn("Interrupted!");
				Thread.currentThread().interrupt();
			}
		}
	}

	private List<Observer> getObservers(String key) {
		List<Observer> observerList = this.observerMap.getList(key);
		if (observerList == null)
			return List.of();

		observerList = new ArrayList<>(observerList);
		if (observerList.isEmpty())
			return List.of();
		return observerList;
	}

	private void notifyAdd(String key, List<StrolchRootElement> elements) {
		List<Observer> observerList = getObservers(key);
		if (observerList.isEmpty())
			return;

		for (Observer observer : observerList) {
			try {
				observer.add(key, elements);
			} catch (Exception e) {
				logger.error("Failed to update observer {} with {} due to {}", key, observer, e.getMessage(), e);
				addLogMessage("add", e);
			}
		}
	}

	private void notifyUpdate(String key, List<StrolchRootElement> elements) {
		List<Observer> observerList = getObservers(key);
		if (observerList.isEmpty())
			return;

		for (Observer observer : observerList) {
			try {
				observer.update(key, elements);
			} catch (Exception e) {
				String msg = "Failed to update observer {0} with {1} due to {2}";
				msg = MessageFormat.format(msg, key, observer, e.getMessage());
				logger.error(msg, e);

				addLogMessage("update", e);
			}
		}
	}

	private void notifyRemove(String key, List<StrolchRootElement> elements) {
		List<Observer> observerList = getObservers(key);
		if (observerList.isEmpty())
			return;

		for (Observer observer : observerList) {
			try {
				observer.remove(key, elements);
			} catch (Exception e) {
				String msg = "Failed to update observer {0} with {1} due to {2}";
				msg = MessageFormat.format(msg, key, observer, e.getMessage());
				logger.error(msg, e);

				addLogMessage("remove", e);
			}
		}
	}

	private void addLogMessage(String type, Exception e) {
		ComponentContainer container = this.agent.getContainer();
		if (container.hasComponent(OperationsLog.class)) {
			OperationsLog operationsLog = container.getComponent(OperationsLog.class);
			operationsLog.addMessage(new LogMessage(this.realm.getRealm(), SYSTEM_USER_AGENT,
					Locator.valueOf(AGENT, ObserverHandler.class.getSimpleName(), type, StrolchAgent.getUniqueId()),
					LogSeverity.Exception, LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"),
					"agent.observers.update.failed").withException(e).value("type", type).value("reason", e));
		}
	}

	@Override
	public void registerObserver(String key, Observer observer) {
		this.observerMap.addElement(key, observer);
		String msg = MessageFormat.format("Registered observer {0} with {1}", key, observer);
		logger.info(msg);
	}

	@Override
	public void unregisterObserver(String key, Observer observer) {
		if (this.observerMap.removeElement(key, observer)) {
			String msg = MessageFormat.format("Unregistered observer {0} with {1}", key, observer);
			logger.info(msg);
		}
	}
}
