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

import ch.eitchnet.privilege.model.Restrictable;

/**
 * Interface for Strolch service's. Service's are the main object in which business logic is implemented in a Strolch
 * agent.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface Service<T extends ServiceArgument, U extends ServiceResult> extends Serializable, Restrictable {

	/**
	 * Performs the actual service
	 * 
	 * @param argument
	 *            the argument for the service
	 * 
	 * @return the service result
	 */
	public U doService(T argument);
}
