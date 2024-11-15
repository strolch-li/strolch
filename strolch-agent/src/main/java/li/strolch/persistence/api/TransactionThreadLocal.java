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

package li.strolch.persistence.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TransactionThreadLocal extends ThreadLocal<StrolchTransaction> {

	private static final Logger logger = LoggerFactory.getLogger(TransactionThreadLocal.class);

	private static final TransactionThreadLocal instance = new TransactionThreadLocal();

	public static boolean hasTx() {
		return instance.get() != null;
	}

	public static StrolchTransaction getTx() {
		StrolchTransaction tx = instance.get();
		if (tx == null)
			throw new IllegalStateException("No TX available on thread " + Thread.currentThread().getName());
		return tx;
	}

	public static void setTx(StrolchTransaction tx) {
		if (instance.get() != null)
			logger.error("THIS THREAD HAS ALREADY OPENED A TX!");
		else
			instance.set(tx);
	}

	public static void removeTx() {
		instance.remove();
	}
}
