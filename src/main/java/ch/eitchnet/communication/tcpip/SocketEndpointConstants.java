package ch.eitchnet.communication.tcpip;

/**
 * Constants used in the communication classes
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SocketEndpointConstants {

	public static final String PARAMETER_USE_TIMEOUT = "useTimeout"; //$NON-NLS-1$
	public static final String PARAMETER_TIMEOUT = "timeout"; //$NON-NLS-1$
	public static final String PARAMETER_RETRY = "retry"; //$NON-NLS-1$
	public static final String PARAMETER_CLEAR_ON_CONNECT = "clearOnConnect"; //$NON-NLS-1$
	public static final String PARAMETER_CONNECT_ON_START = "connectOnStart"; //$NON-NLS-1$
	public static final String PARAMETER_CLOSE_AFTER_SEND = "closeAfterSend"; //$NON-NLS-1$

	public static final String PARAMETER_REMOTE_OUTPUT_PORT = "remoteOutputPort"; //$NON-NLS-1$
	public static final String PARAMETER_REMOTE_OUTPUT_ADDRESS = "remoteOutputAddress"; //$NON-NLS-1$
	public static final String PARAMETER_LOCAL_INPUT_PORT = "localInputPort"; //$NON-NLS-1$
	public static final String PARAMETER_LOCAL_INPUT_ADDRESS = "localInputAddress"; //$NON-NLS-1$

	public static final String PARAMETER_LOCAL_OUTPUT_ADDRESS = "localOutputAddress"; //$NON-NLS-1$
	public static final String PARAMETER_LOCAL_OUTPUT_PORT = "localOutputPort"; //$NON-NLS-1$
	public static final String PARAMETER_REMOTE_INPUT_ADDRESS = "remoteInputAddress"; //$NON-NLS-1$
	public static final String PARAMETER_REMOTE_INPUT_PORT = "remoteInputPort"; //$NON-NLS-1$

	/**
	 * Time to wait in milliseconds before reestablishing a connection. Default is 60000ms
	 */
	public static final long RETRY = 60000l;

	/**
	 * The time after which a connection is deemed dead. Value is 60000ms
	 */
	public static final int TIMEOUT = 60000;

	/**
	 * Default is to use a timeout on socket connections, thus this value is true
	 */
	public static final boolean USE_TIMEOUT = true;

	/**
	 * Default is to not clear the input socket on connect, thus this value is false
	 */
	public static final boolean CLEAR_ON_CONNECT = false;

	/**
	 * Default is to connect on start of the connection
	 */
	public static final boolean CONNECT_ON_START = true;
	
	/**
	 * Default is to not close after sending
	 */
	public static final boolean CLOSE_AFTER_SEND = false;

	/**
	 * Default is to disconnect after a null message is received when reading from a TCP socket, thus this value is true
	 */
	public static final boolean DISCONNECT_ON_NULL_MSG = true;

	/**
	 * If {@link #DISCONNECT_ON_NULL_MSG} is activated, then this is the default time used to wait before reading again,
	 * which is 10000ms
	 */
	public static final long WAIT_TIME_ON_NULL_MSG = 10000l;
}
