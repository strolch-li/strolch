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
package ch.eitchnet.communication;

import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TestConnectionObserver implements ConnectionObserver {

	private static final Logger logger = LoggerFactory.getLogger(FileEndpointTest.class);

	private IoMessage message;

	public IoMessage getMessage() {
		return this.message;
	}

	@Override
	public void notify(CommandKey key, IoMessage message) {
		this.message = message;
		logger.info(MessageFormat.format("Received message with key {0} and message {1}", key, message)); //$NON-NLS-1$
	}
}