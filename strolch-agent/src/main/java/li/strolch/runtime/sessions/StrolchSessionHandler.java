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
package li.strolch.runtime.sessions;

import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Usage;

import java.util.List;
import java.util.Locale;

/**
 * The {@link StrolchSessionHandler} implements session management. It authenticates, validates and invalidates session
 * depending on the concrete implementation
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchSessionHandler {

	/**
	 * Refreshes the sessions from the {@link li.strolch.runtime.privilege.PrivilegeHandler}
	 */
	void refreshSessions();

	/**
	 * Returns the time to live for a session in minutes
	 *
	 * @return the time to live for a session in minutes
	 */
	int getSessionTtlMinutes();

	/**
	 * Returns the max keep alive for a session in minutes
	 *
	 * @return the max keep alive for a session in minutes
	 */
	int getSessionMaxKeepAliveMinutes();

	/**
	 * Return true if refreshing sessions is allowed
	 *
	 * @return true if refreshing sessions is allowed
	 */
	boolean isRefreshAllowed();

	/**
	 * Authenticates a user with the given credentials
	 *
	 * @param username
	 * 		the username
	 * @param password
	 * 		the password
	 * @param source
	 * 		the source of the request
	 * @param usage
	 * 		the usage for this authentication
	 * @param keepAlive
	 * 		should the session have a keepAlive
	 *
	 * @return the {@link Certificate} for the logged in user
	 */
	Certificate authenticate(String username, char[] password, String source, Usage usage, boolean keepAlive);

	/**
	 * Performs a single-sign-on with the given data, if SSO is enabled
	 *
	 * @param data
	 * 		the data to pass to the SSO handler
	 *
	 * @return the {@link Certificate} for the logged in user
	 */
	Certificate authenticateSingleSignOn(Object data);

	/**
	 * Performs a single-sign-on with the given data, if SSO is enabled
	 *
	 * @param data
	 * 		the data to pass to the SSO handler
	 * @param source
	 * 		the source of the request
	 *
	 * @return the {@link Certificate} for the logged in user
	 */
	Certificate authenticateSingleSignOn(Object data, String source);

	/**
	 * Performs a refresh of the given certificate's session by returning a new certificate
	 *
	 * @param certificate
	 * 		the certificate to refresh
	 * @param source
	 * 		the source of the request
	 *
	 * @return certificate a new certificate
	 */
	Certificate refreshSession(Certificate certificate, String source);

	/**
	 * Returns true if a {@link Certificate} exists with the given auth token
	 *
	 * @param authToken
	 * 		the auth token for the certificate
	 *
	 * @return true if a {@link Certificate} exists with the given auth token
	 */
	boolean isSessionKnown(String authToken);

	/**
	 * Validates that a {@link Certificate} exists with the given auth token and is still valid
	 *
	 * @param authToken
	 * 		the auth token for the certificate
	 *
	 * @return the {@link Certificate} for the given auth token
	 *
	 * @throws StrolchNotAuthenticatedException
	 * 		if no logged in user exists with the given auth token
	 */
	Certificate validate(String authToken) throws StrolchNotAuthenticatedException;

	/**
	 * Validates that a {@link Certificate} exists with the given auth token and is still valid
	 *
	 * @param authToken
	 * 		the auth token for the certificate
	 * @param source
	 * 		the source of the request
	 *
	 * @return the {@link Certificate} for the given auth token
	 *
	 * @throws StrolchNotAuthenticatedException
	 * 		if no logged in user exists with the given auth token
	 */
	Certificate validate(String authToken, String source) throws StrolchNotAuthenticatedException;

	/**
	 * Validate that the given {@link Certificate} is still valid
	 *
	 * @param certificate
	 * 		the certificate to validate
	 *
	 * @return the {@link PrivilegeContext} for the given certificate to perform authorization checks against
	 *
	 * @throws StrolchNotAuthenticatedException
	 * 		if no logged in user exists with the given auth token
	 */
	PrivilegeContext validate(Certificate certificate) throws StrolchNotAuthenticatedException;

	/**
	 * Validate that the given {@link Certificate} is still valid
	 *
	 * @param certificate
	 * 		the certificate to validate
	 * @param source
	 * 		the source of the request
	 *
	 * @return the {@link PrivilegeContext} for the given certificate to perform authorization checks against
	 *
	 * @throws StrolchNotAuthenticatedException
	 * 		if no logged in user exists with the given auth token
	 */
	PrivilegeContext validate(Certificate certificate, String source) throws StrolchNotAuthenticatedException;

	/**
	 * Returns all the {@link UserSession}
	 *
	 * @param certificate
	 * 		the certificate to validate if the requester may perform this action
	 * @param source
	 * 		the source of the request
	 *
	 * @return the list of {@link UserSession}
	 */
	List<UserSession> getSessions(Certificate certificate, String source);

	/**
	 * Return the {@link UserSession} with the given sessionId
	 *
	 * @param certificate
	 * 		the certificate to validate if the requester may perform this action
	 * @param source
	 * 		the source of the request
	 * @param sessionId
	 * 		the id of the {@link UserSession} to return
	 *
	 * @return the user session
	 *
	 * @throws AccessDeniedException
	 * 		if the given {@link Certificate} may not access the {@link UserSession}
	 * @throws PrivilegeException
	 * 		if the {@link UserSession} does not exist, or another issues arises
	 */
	UserSession getSession(Certificate certificate, String source, String sessionId)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * Invalidates the given certificate
	 *
	 * @param certificate
	 * 		the certificate to invalidate
	 */
	void invalidate(Certificate certificate);

	/**
	 * Invalidates the {@link Certificate} with the given sessionId
	 *
	 * @param certificate
	 * 		the certificate of the user requesting to invalidate the requested certificate
	 */
	void invalidate(Certificate certificate, String sessionId);

	/**
	 * Set the locale of the given sessionId to the given locale
	 *
	 * @param certificate
	 * 		the certificate of the user requesting to invalidate the requested certificate
	 * @param sessionId
	 * 		the ID of the session on which to set the locale
	 * @param locale
	 * 		the locale to set
	 */
	void setSessionLocale(Certificate certificate, String sessionId, Locale locale);

	/**
	 * Initiate a password reset challenge for the given username
	 *
	 * @param usage
	 * 		the usage for which the challenge is requested
	 * @param username
	 * 		the username of the user to initiate the challenge for
	 */
	void initiateChallengeFor(Usage usage, String username);

	/**
	 * Initiate a password reset challenge for the given username
	 *
	 * @param usage
	 * 		the usage for which the challenge is requested
	 * @param username
	 * 		the username of the user to initiate the challenge for
	 * @param source
	 * 		the source of the request
	 */
	void initiateChallengeFor(Usage usage, String username, String source);

	/**
	 * Validate the response of a challenge for the given username
	 *
	 * @param username
	 * 		the username of the user for which the challenge is to be validated
	 * @param challenge
	 * 		the challenge from the user
	 *
	 * @return certificate with which the user can access the system with the {@link Usage} set to the value from the
	 * initiated challenge
	 *
	 * @throws PrivilegeException
	 * 		if anything goes wrong
	 */
	Certificate validateChallenge(String username, String challenge) throws PrivilegeException;

	/**
	 * Validate the response of a challenge for the given username
	 *
	 * @param username
	 * 		the username of the user for which the challenge is to be validated
	 * @param challenge
	 * 		the challenge from the user
	 * @param source
	 * 		the source of the request
	 *
	 * @return certificate with which the user can access the system with the {@link Usage} set to the value from the
	 * initiated challenge
	 *
	 * @throws PrivilegeException
	 * 		if anything goes wrong
	 */
	Certificate validateChallenge(String username, String challenge, String source) throws PrivilegeException;
}
