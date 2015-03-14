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
package li.strolch.rest;

import java.util.List;
import java.util.Locale;

import li.strolch.rest.model.UserSession;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchSessionHandler {

	public Certificate authenticate(String username, byte[] password);

	public Certificate validate(String authToken);

	public Certificate validate(Certificate certificate);

	public void invalidate(Certificate certificate);

	public List<UserSession> getSessions(Certificate certificate);

	public UserSession getSession(Certificate certificate, String sessionId);

	public void invalidateSession(Certificate certificate, String sessionId);

	public void setSessionLocale(Certificate certificate, String sessionId, Locale locale);
}
