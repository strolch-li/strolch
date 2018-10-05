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
package li.strolch.communication;

/**
 * <p>
 * a {@link CommunicationConnection} undergoes a serious of state changes. These states can be viewed by a client to
 * monitor the state of the connection
 * </p>
 * The states have the following semantics:
 * <ul>
 * <li>CREATED - initial state</li>
 * <li>INITIALIZED - the appropriate connection parameters are found</li>
 * <li>CONNECTING - the connection is trying to build up a connection</li>
 * <li>WAITING - the connection is waiting before retrying to connect</li>
 * <li>CONNECTED - the connection has just been established</li>
 * <li>IDLE - the connection is connected, but waiting for work</li>
 * <li>WORKING - the connection is working</li>
 * <li>BROKEN - the connection has lost the connection and is waiting before reconnecting, or another unknown failure
 * occurred</li>
 * <li>DISCONNECTED - the connection has been disconnected</li>
 * </ul>
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public enum ConnectionState {

	// initial
	CREATED,
	// configured and ready to connect
	INITIALIZED,
	// working
	CONNECTING,
	WAITING,
	CONNECTED,
	IDLE,
	WORKING,
	BROKEN,
	// disconnected due to connection error or manual disconnect/stop
	DISCONNECTED;
}
