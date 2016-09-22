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
package li.strolch.communication;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
@XmlRootElement(name = "ConnectionInfo")
@XmlAccessorType(XmlAccessType.FIELD)
public class ConnectionInfo {

	@XmlAttribute(name = "id")
	private String id;

	@XmlAttribute(name = "localUri")
	private String localUri;

	@XmlAttribute(name = "remoteUri")
	private String remoteUri;

	@XmlAttribute(name = "mode")
	private ConnectionMode mode;

	@XmlAttribute(name = "queueSize")
	private int queueSize;

	@XmlAttribute(name = "state")
	private ConnectionState state;

	@XmlAttribute(name = "stateMsg")
	private String stateMsg;

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the localUri
	 */
	public String getLocalUri() {
		return this.localUri;
	}

	/**
	 * @param localUri
	 *            the localUri to set
	 */
	public void setLocalUri(String localUri) {
		this.localUri = localUri;
	}

	/**
	 * @return the remoteUri
	 */
	public String getRemoteUri() {
		return this.remoteUri;
	}

	/**
	 * @param remoteUri
	 *            the remoteUri to set
	 */
	public void setRemoteUri(String remoteUri) {
		this.remoteUri = remoteUri;
	}

	/**
	 * @return the mode
	 */
	public ConnectionMode getMode() {
		return this.mode;
	}

	/**
	 * @param mode
	 *            the mode to set
	 */
	public void setMode(ConnectionMode mode) {
		this.mode = mode;
	}

	/**
	 * @return the state
	 */
	public ConnectionState getState() {
		return this.state;
	}

	/**
	 * @param state
	 *            the state to set
	 */
	public void setState(ConnectionState state) {
		this.state = state;
	}

	/**
	 * @return the stateMsg
	 */
	public String getStateMsg() {
		return this.stateMsg;
	}

	/**
	 * @param stateMsg
	 *            the stateMsg to set
	 */
	public void setStateMsg(String stateMsg) {
		this.stateMsg = stateMsg;
	}

	/**
	 * @return the queueSize
	 */
	public int getQueueSize() {
		return this.queueSize;
	}

	/**
	 * @param queueSize
	 *            the queueSize to set
	 */
	public void setQueueSize(int queueSize) {
		this.queueSize = queueSize;
	}

	public static ConnectionInfo valueOf(CommunicationConnection connection) {
		ConnectionInfo info = new ConnectionInfo();
		info.setId(connection.getId());
		info.setLocalUri(connection.getLocalUri());
		info.setRemoteUri(connection.getRemoteUri());
		info.setMode(connection.getMode());
		info.setState(connection.getState());
		info.setStateMsg(connection.getStateMsg());
		info.setQueueSize(connection.getQueueSize());
		return info;
	}
}
