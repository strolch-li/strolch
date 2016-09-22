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

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.utils.helper.StringHelper;

/**
 * Helper class to thrown connection messages
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ConnectionMessages {

	private static Logger logger = LoggerFactory.getLogger(ConnectionMessages.class);

	/**
	 * Utility class
	 */
	private ConnectionMessages() {
		//
	}

	/**
	 * Convenience method to throw an exception when an illegal {@link ConnectionState} change occurs
	 * 
	 * @param current
	 *            the current state
	 * @param change
	 *            the new state
	 * 
	 * @return the exception
	 */
	public static ConnectionException throwIllegalConnectionState(ConnectionState current, ConnectionState change) {
		String msg = "The connection with state {0} cannot be changed to {1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, current.name(), change.name());
		ConnectionException e = new ConnectionException(msg);
		return e;
	}

	/**
	 * Convenience method to throw an exception when an invalid parameter is set in the configuration
	 * 
	 * @param clazz
	 *            clazz type
	 * @param parameterName
	 *            the name of the parameter
	 * @param parameterValue
	 *            the value of the parameter
	 * 
	 * @return the exception
	 */
	public static ConnectionException throwInvalidParameter(Class<?> clazz, String parameterName,
			String parameterValue) {
		String value;
		if (parameterValue != null && !parameterValue.isEmpty())
			value = parameterValue;
		else
			value = StringHelper.NULL;

		String msg = "{0}: parameter ''{1}'' has invalid value ''{2}''"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, clazz.getSimpleName(), parameterName, value);
		ConnectionException e = new ConnectionException(msg);
		return e;
	}

	/**
	 * Convenience method to throw an exception when an two conflicting parameters are activated
	 * 
	 * @param clazz
	 *            clazz type
	 * @param parameter1
	 *            parameter 1
	 * @param parameter2
	 *            parameter 2
	 * 
	 * @return the exception
	 */
	public static ConnectionException throwConflictingParameters(Class<?> clazz, String parameter1, String parameter2) {
		String msg = "{0} : The parameters {1} and {2} can not be both activated as they conflict"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, clazz.getSimpleName(), parameter1, parameter1);
		ConnectionException e = new ConnectionException(msg);
		return e;
	}

	/**
	 * Convenience method to log a warning when a parameter is not set in the configuration
	 * 
	 * @param clazz
	 *            the clazz of the warning
	 * @param parameterName
	 *            the parameter name
	 * @param defValue
	 *            the default value to be used
	 */
	public static void warnUnsetParameter(Class<?> clazz, String parameterName, String defValue) {
		if (logger.isDebugEnabled()) {
			String msg = "{0}: parameter ''{1}'' is not set, using default value ''{2}''"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, clazz.getSimpleName(), parameterName, defValue);
			Map<String, String> properties = new HashMap<>();
			logger.warn(MessageFormat.format(msg, properties));
		}
	}

	/**
	 * Convenience method to throw an exception when the connection is not yet configured
	 * 
	 * @param connection
	 *            the connection
	 * @param message
	 *            the message
	 */
	public static void assertConfigured(CommunicationConnection connection, String message) {
		if (connection.getState() == ConnectionState.CREATED) {
			String msg = "{0} : Not yet configured: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, connection.getId(), message);
			throw new ConnectionException(msg);
		}
	}

	/**
	 * Convenience method to throw an exception when the connection is not connected
	 * 
	 * @param connection
	 *            the connection
	 * @param message
	 *            the message
	 * 
	 * @return the exception
	 */
	public static ConnectionException throwNotConnected(CommunicationConnection connection, String message) {
		String msg = "{0} : Not connected: {1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, connection.getId(), message);
		ConnectionException e = new ConnectionException(msg);
		return e;
	}

	/**
	 * Convenience method to throw an exception when the connection is not connected
	 * 
	 * @param connection
	 *            the connection
	 * @param message
	 *            the message
	 * 
	 * @return the exception
	 */
	public static ConnectionException throwNotConnected(CommunicationConnection connection, IoMessage message) {
		String msg = "{0} : Not connected, can not send message with id {1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, connection.getId(), message.getId());
		ConnectionException e = new ConnectionException(msg);
		return e;
	}

	public static void assertLegalMessageVisitor(Class<? extends CommunicationEndpoint> endpoint,
			Class<? extends IoMessageVisitor> expectedVisitor, IoMessageVisitor actualVisitor) {
		if (!(expectedVisitor.isAssignableFrom(actualVisitor.getClass()))) {
			String msg = "{0} requires {1} but has received illegal type {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, endpoint.getName(), expectedVisitor.getName(),
					actualVisitor.getClass().getName());
			throw new ConnectionException(msg);
		}
	}
}