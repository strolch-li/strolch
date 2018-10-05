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

import java.util.*;

public class SimpleMessageArchive implements IoMessageArchive {

	private int maxSize;
	private int trimSize;
	private TreeSet<IoMessage> messageArchive;

	public SimpleMessageArchive() {
		this(1000, 100);
	}

	public SimpleMessageArchive(int maxSize, int trimSize) {
		this.maxSize = maxSize;
		this.trimSize = trimSize;

		this.messageArchive = new TreeSet<>(new Comparator<IoMessage>() {
			@Override
			public int compare(IoMessage o1, IoMessage o2) {
				return o1.getUpdated().compareTo(o2.getUpdated());
			}
		});
	}

	@Override
	public synchronized int getMaxSize() {
		return this.maxSize;
	}

	@Override
	public synchronized void setMaxSize(int maxSize) {
		this.maxSize = maxSize;
		trim();
	}

	@Override
	public synchronized int getTrimSize() {
		return this.trimSize;
	}

	@Override
	public synchronized void setTrimSize(int trimSize) {
		this.trimSize = trimSize;
	}

	@Override
	public synchronized int size() {
		return this.messageArchive.size();
	}

	@Override
	public synchronized List<IoMessage> getAll() {
		List<IoMessage> all = new ArrayList<>(this.messageArchive);
		return all;
	}

	@Override
	public synchronized List<IoMessage> getBy(String connectionId) {
		List<IoMessage> all = new ArrayList<>();
		for (IoMessage msg : this.messageArchive) {
			if (msg.getConnectionId().equals(connectionId))
				all.add(msg);
		}
		return all;
	}

	@Override
	public synchronized List<IoMessage> getBy(String connectionId, CommandKey key) {
		List<IoMessage> all = new ArrayList<>();
		for (IoMessage msg : this.messageArchive) {
			if (msg.getConnectionId().equals(connectionId) && msg.getKey().equals(key))
				all.add(msg);
		}
		return all;
	}

	@Override
	public synchronized void clearArchive() {
		this.messageArchive.clear();
	}

	@Override
	public synchronized void archive(IoMessage message) {
		this.messageArchive.add(message);
		trim();
	}

	protected void trim() {
		if (this.messageArchive.size() <= this.maxSize)
			return;

		Iterator<IoMessage> iter = this.messageArchive.iterator();
		for (int i = 0; i <= this.trimSize; i++) {
			if (iter.hasNext()) {
				iter.next();
				iter.remove();
			}
		}
	}
}
