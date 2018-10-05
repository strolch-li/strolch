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

import java.util.List;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class TestIoMessage extends IoMessage {

	private List<String> contents;

	public TestIoMessage(String id, CommandKey key, String connectionId) {
		super(id, key, connectionId);
	}

	public TestIoMessage(String id, CommandKey key, String connectionId, List<String> contents) {
		super(id, key, connectionId);
		this.contents = contents;
	}

	public List<String> getContents() {
		return this.contents;
	}

	public void setContents(List<String> contents) {
		this.contents = contents;
	}
}
