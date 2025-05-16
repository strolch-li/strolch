/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils;

import java.util.concurrent.ThreadFactory;

/**
 * Simple {@link ThreadFactory} which allocates as a pool and has a name for each pool
 */
public class NamedThreadPoolFactory implements ThreadFactory {
	private final ThreadFactory factory;

	public NamedThreadPoolFactory(String poolName) {
		this.factory = Thread.ofPlatform().name(poolName + "-", 0).factory();
	}

	@Override
	public Thread newThread(Runnable r) {
		Thread t = this.factory.newThread(r);
		if (t.getPriority() != Thread.NORM_PRIORITY)
			t.setPriority(Thread.NORM_PRIORITY);
		return t;
	}
}