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
package li.strolch.service.test;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class GreetingResult extends ServiceResult {
	private static final long serialVersionUID = 1L;
	private String greeting;

	public GreetingResult() {
		super(ServiceResultState.SUCCESS);
	}

	/**
	 * @return the greeting
	 */
	public String getGreeting() {
		return this.greeting;
	}

	/**
	 * @param greeting
	 *            the greeting to set
	 */
	public void setGreeting(String greeting) {
		this.greeting = greeting;
	}
}