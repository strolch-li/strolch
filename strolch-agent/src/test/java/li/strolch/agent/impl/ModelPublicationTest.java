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
package li.strolch.agent.impl;

import org.junit.Test;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.Assert.*;

public class ModelPublicationTest {
	@Test
	public void queryCannotReturnHalfPublishedBatch() throws Exception {
		ModelPublication publication = new ModelPublication();
		AtomicInteger first = new AtomicInteger();
		AtomicInteger second = new AtomicInteger();
		CountDownLatch halfPublished = new CountDownLatch(1);
		CountDownLatch complete = new CountDownLatch(1);
		CountDownLatch readAttempt = new CountDownLatch(1);
		try (var executor = Executors.newFixedThreadPool(2)) {
			var writer = executor.submit(() -> publication.publish(() -> {
				first.set(1);
				halfPublished.countDown();
				await(complete);
				second.set(1);
			}));
			assertTrue(halfPublished.await(5, TimeUnit.SECONDS));
			var reader = executor.submit(() -> publication.read(() -> {
				List<Integer> pair = List.of(first.get(), second.get());
				readAttempt.countDown();
				return pair;
			}));
			try {
				assertTrue(readAttempt.await(5, TimeUnit.SECONDS));
				assertFalse(reader.isDone());
			} finally {
				complete.countDown();
			}
			writer.get(5, TimeUnit.SECONDS);
			assertEquals(List.of(1, 1), reader.get(5, TimeUnit.SECONDS));
		}
	}

	@Test
	public void affectedPointReadWaitsButUnrelatedPointReadDoesNot() throws Exception {
		ModelPublication publication = new ModelPublication();
		var values = new java.util.concurrent.ConcurrentHashMap<String, Integer>();
		values.put("first", 0);
		values.put("unrelated", 0);
		CountDownLatch halfPublished = new CountDownLatch(1);
		CountDownLatch complete = new CountDownLatch(1);
		CountDownLatch readAttempt = new CountDownLatch(1);
		try (var executor = Executors.newFixedThreadPool(2)) {
			var writer = executor.submit(() -> publication.publish(() -> {
				values.put("first", 1);
				halfPublished.countDown();
				await(complete);
			}, (_, _, id) -> id.equals("first")));
			assertTrue(halfPublished.await(5, TimeUnit.SECONDS));
			try {
				assertEquals(Integer.valueOf(0), publication.readRoot("kind", "type", "unrelated", () -> values.get("unrelated")));
				var reader = executor.submit(() -> publication.readRoot("kind", "type", "first", () -> {
					int value = values.get("first");
					readAttempt.countDown();
					return value;
				}));
				assertTrue(readAttempt.await(5, TimeUnit.SECONDS));
				assertFalse(reader.isDone());
				complete.countDown();
				assertEquals(Integer.valueOf(1), reader.get(5, TimeUnit.SECONDS));
			} finally {
				complete.countDown();
			}
			writer.get(5, TimeUnit.SECONDS);
		}
	}

	@Test
	public void partialPublicationFencesReadsAndRethrowsOriginalFailure() {
		for (Throwable failure : List.of(new IllegalStateException("publication failure"), new AssertionError("publication error"))) {
			ModelPublication publication = new ModelPublication();
			Throwable thrown = assertThrows(failure.getClass(), () -> publication.publish(() -> {
				if (failure instanceof RuntimeException exception) throw exception;
				throw (Error) failure;
			}));
			assertSame(failure, thrown);
			IllegalStateException fenced = assertThrows(IllegalStateException.class, () -> publication.read(() -> "stale"));
			assertSame(failure, fenced.getCause());
			assertThrows(IllegalStateException.class, () -> publication.readRoot("kind", "type", "id", () -> "stale"));
		}
	}

	private static void await(CountDownLatch latch) {
		try { assertTrue(latch.await(5, TimeUnit.SECONDS)); }
		catch (InterruptedException e) { Thread.currentThread().interrupt(); throw new AssertionError(e); }
	}
}
