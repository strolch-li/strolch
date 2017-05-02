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
package li.strolch.agent.api;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;

/**
 * <p>
 * Implementing the observer pattern by allowing {@link Observer} to register themselves for updates to
 * {@link StrolchRootElement}
 * </p>
 * 
 * <p>
 * Note: The key in all the methods can be any string, but as a convenience it is mostly one of the following:
 * </p>
 * <ul>
 * <li>{@link Tags#RESOURCE}</li>
 * <li>{@link Tags#ORDER}</li>
 * <li>{@link Tags#ACTIVITY}</li>
 * </ul>
 * 
 * <p>
 * Should a special case arise, then it a contract must be defined by which a key is negotiated between an event
 * distributer and the observer
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ObserverHandler {

	/**
	 * Update observers with the given event
	 * 
	 * @param observerEvent
	 *            containing the updates
	 */
	public void notify(ObserverEvent observerEvent);

	/**
	 * Registers the {@link Observer} for notification of objects under the given key
	 * 
	 * @param key
	 *            the key for which to the observer wants to be notified
	 * @param observer
	 *            the observer to register
	 */
	public void registerObserver(String key, Observer observer);

	/**
	 * Unregister the given {@link Observer}
	 * 
	 * @param key
	 *            the key for which to the observer was registered
	 * @param observer
	 *            the observer unregister
	 */
	public void unregisterObserver(String key, Observer observer);

	/**
	 * Start the update thread
	 */
	public void start();

	/**
	 * Stop the update thread
	 */
	public void stop();
}
