package ch.eitchnet.communication;

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
 * @author Robert von Burg <eitch@eitchnet.ch>
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
