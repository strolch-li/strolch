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
package li.strolch.test.benchmark;

import li.strolch.utils.concurrent.ElementLockingException;
import org.junit.Test;
import java.io.Serial;

import static org.junit.Assert.assertEquals;

public class TransientTransactionBenchmarkTest {
	@Test
	public void recognizesWrappedLockExceptionSubclasses() {
		class SpecificLockException extends ElementLockingException {
			@Serial private static final long serialVersionUID = 1L;
			SpecificLockException() { super("locked"); }
		}
		RuntimeException failure = new IllegalStateException("transaction failed", new SpecificLockException());
		assertEquals(2, TransientTransactionBenchmark.failureOutcome(failure, null));
	}

	@Test
	public void recognizesOptionalConflictTypeAndSubclasses() {
		class LegacyConflictException extends RuntimeException {
			@Serial private static final long serialVersionUID = 1L;
		}
		class SpecificConflictException extends LegacyConflictException {
			@Serial private static final long serialVersionUID = 1L;
		}
		RuntimeException failure = new IllegalStateException("transaction failed", new SpecificConflictException());
		assertEquals(1, TransientTransactionBenchmark.failureOutcome(failure, LegacyConflictException.class));
		assertEquals(-1, TransientTransactionBenchmark.failureOutcome(failure, null));
	}

	@Test
	public void doesNotCountArbitraryFailuresAsConflicts() {
		assertEquals(-1, TransientTransactionBenchmark.failureOutcome(new IllegalArgumentException("unexpected"), null));
	}
}
