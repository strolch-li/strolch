/*
 * Copyright 2014 Robert von Burg <eitch@eitchnet.ch>
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
package ch.eitchnet.communication;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * <p>
 * An {@link IoMessage} is the object containing the data to be transmitted in IO. Implementations of
 * {@link CommunicationConnection} should implement sub classes of this method to hold the actual payload.
 * </p>
 * 
 * <p>
 * This class also contains a {@link Map} to store transient meta data to the actual payload
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IoMessage {

	private final String id;
	private final CommandKey key;
	private final String connectionId;
	private Date updated;
	private State state;
	private String stateMsg;
	private Map<String, Object> parameters;

	/**
	 * @param id
	 * @param key
	 * @param connectionId
	 */
	public IoMessage(String id, CommandKey key, String connectionId) {
		this.id = id;
		this.key = key;
		this.connectionId = connectionId;
		this.state = State.CREATED;
		this.stateMsg = StringHelper.DASH;
		this.updated = new Date();
		this.parameters = new HashMap<>();
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * @return the key
	 */
	public CommandKey getKey() {
		return this.key;
	}

	/**
	 * @return the connectionId
	 */
	public String getConnectionId() {
		return this.connectionId;
	}

	/**
	 * @return the updated
	 */
	public Date getUpdated() {
		return this.updated;
	}

	/**
	 * Used for testing purposes only!
	 * 
	 * @param date
	 */
	void setUpdated(Date date) {
		this.updated = date;
	}

	/**
	 * @return the state
	 */
	public State getState() {
		return this.state;
	}

	/**
	 * @return the stateMsg
	 */
	public String getStateMsg() {
		return this.stateMsg;
	}

	/**
	 * @param state
	 *            the state
	 * @param stateMsg
	 *            the stateMsg
	 */
	public void setState(State state, String stateMsg) {
		this.state = state;
		this.stateMsg = stateMsg;
		this.updated = new Date();
	}

	@SuppressWarnings("unchecked")
	public <T> T getParam(String key) {
		return (T) this.parameters.get(key);
	}

	/**
	 * Add a transient parameter to this message
	 * 
	 * @param key
	 *            the key under which the parameter is to be stored
	 * @param value
	 *            the value to store under the given key
	 */
	public void addParam(String key, Object value) {
		this.parameters.put(key, value);
	}

	/**
	 * Removes the parameter with the given key
	 * 
	 * @param key
	 *            The give of the parameter to be removed
	 * @return the removed value, or null if the object didn't exist
	 */
	@SuppressWarnings("unchecked")
	public <T> T removeParam(String key) {
		return (T) this.parameters.remove(key);
	}

	/**
	 * @return the set of parameter keys
	 */
	public Set<String> getParamKeys() {
		return this.parameters.keySet();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(this.getClass().getSimpleName() + " [id=");
		builder.append(this.id);
		builder.append(", key=");
		builder.append(this.key);
		builder.append(", updated=");
		builder.append(ISO8601FormatFactory.getInstance().formatDate(this.updated));
		builder.append(", state=");
		builder.append(this.state);
		if (StringHelper.isNotEmpty(this.stateMsg)) {
			builder.append(", stateMsg=");
			builder.append(this.stateMsg);
		}
		builder.append("]");
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		IoMessage other = (IoMessage) obj;
		if (this.id == null) {
			if (other.id != null)
				return false;
		} else if (!this.id.equals(other.id))
			return false;
		return true;
	}

	public enum State {
		CREATED, // new
		PENDING, // outbound
		ACCEPTED, // inbound
		DONE, // completed
		FAILED, // completed
		FATAL; // not sendable
	}
}
