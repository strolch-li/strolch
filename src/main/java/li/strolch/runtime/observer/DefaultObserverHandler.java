/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.observer;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.model.StrolchElement;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class DefaultObserverHandler extends StrolchComponent implements ObserverHandler {

	private Map<String, List<Observer>> observerMap;

	@Override
	public void initialize(ComponentConfiguration configuration) {
		this.observerMap = new HashMap<>();
		super.initialize(configuration);
	}

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultObserverHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void add(String key, List<StrolchElement> elements) {
		List<Observer> observerList = this.observerMap.get(key);
		if (observerList != null && !observerList.isEmpty()) {
			for (Observer observer : observerList) {
				try {
					observer.add(key, elements);
				} catch (Exception e) {
					String msg = "Failed to update observer {0} with {1} due to {2}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, key, observer, e.getMessage());
					logger.error(msg, e);
				}
			}
		}
	}

	@Override
	public void update(String key, List<StrolchElement> elements) {
		List<Observer> observerList = this.observerMap.get(key);
		if (observerList != null && !observerList.isEmpty()) {
			for (Observer observer : observerList) {
				try {
					observer.update(key, elements);
				} catch (Exception e) {
					String msg = "Failed to update observer {0} with {1} due to {2}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, key, observer, e.getMessage());
					logger.error(msg, e);
				}
			}
		}
	}

	@Override
	public void remove(String key, List<StrolchElement> elements) {
		List<Observer> observerList = this.observerMap.get(key);
		if (observerList != null && !observerList.isEmpty()) {
			for (Observer observer : observerList) {
				try {
					observer.remove(key, elements);
				} catch (Exception e) {
					String msg = "Failed to update observer {0} with {1} due to {2}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, key, observer, e.getMessage());
					logger.error(msg, e);
				}
			}
		}
	}

	@Override
	public void registerObserver(String key, Observer observer) {
		List<Observer> observerList = this.observerMap.get(key);
		if (observerList == null) {
			observerList = new ArrayList<>();
			this.observerMap.put(key, observerList);
		}
		observerList.add(observer);
		String msg = MessageFormat.format("Registered observer {0} with {1}", key, observer); //$NON-NLS-1$
		logger.info(msg);
	}

	@Override
	public void unregisterObserver(String key, Observer observer) {
		List<Observer> observerList = this.observerMap.get(key);
		if (observerList != null && observerList.remove(observer)) {
			String msg = MessageFormat.format("Unregistered observer {0} with {1}", key, observer); //$NON-NLS-1$
			logger.info(msg);
		}
	}
}
