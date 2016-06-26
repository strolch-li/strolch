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
package li.strolch.communication;

import li.strolch.communication.IoMessage.State;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IoMessageStateObserver implements ConnectionObserver {

	private CommandKey key;
	private String messageId;
	private State state;

	public IoMessageStateObserver(CommandKey key, String messageId) {
		this.key = key;
		this.messageId = messageId;
	}

	@Override
	public void notify(CommandKey key, IoMessage message) {
		if (this.key.equals(key) && message.getId().equals(this.messageId)) {
			this.state = message.getState();

			synchronized (this) {
				notifyAll();
			}
		}
	}

	public State getState() {
		return this.state;
	}
}
