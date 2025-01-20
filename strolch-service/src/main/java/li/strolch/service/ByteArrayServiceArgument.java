/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.service;

import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;

import java.util.HashMap;
import java.util.Map;

/**
 * A {@link ServiceArgument} which takes a byte array as the input for a {@link AbstractService}.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ByteArrayServiceArgument extends ServiceArgument {

	/**
	 * the objectType - its context is defined by the service
	 */
	public String objectType;

	/**
	 * the objectId - its context is defined by the service
	 */
	public String objectId;

	/**
	 * Additional arguments relevant to the service
	 */
	public final Map<String, String> map = new HashMap<>();

	/**
	 * The input object
	 */
	public byte[] bytes;
}
