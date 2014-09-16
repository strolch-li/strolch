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
package li.strolch.service.api;

import java.io.Serializable;

import li.strolch.runtime.StrolchConstants;

/**
 * Base argument to be used when performing {@link Service Services}. The realm parameter is set to
 * {@link StrolchConstants#DEFAULT_REALM} and can be overridden when the caller of the service wants to perform the
 * service in a different realm
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServiceArgument implements Serializable {
	private static final long serialVersionUID = 1L;

	/**
	 * <p>
	 * Set this to the realm in which the service should operate
	 * </p>
	 * 
	 * <p>
	 * realm = StrolchConstants.DEFAULT_REALM
	 * </p>
	 */
	public String realm = StrolchConstants.DEFAULT_REALM;
}
