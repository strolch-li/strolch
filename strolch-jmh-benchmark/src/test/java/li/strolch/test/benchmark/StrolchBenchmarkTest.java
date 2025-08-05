/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

import org.junit.Ignore;
import org.junit.Test;
import org.openjdk.jmh.profile.*;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.ChainedOptionsBuilder;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.VerboseMode;

public class StrolchBenchmarkTest {

	@Test
	public void runAllBenchmarks() throws Exception {
		Options opt = getOptionsBuilder()

				.include(PerformanceTransientTest.class.getSimpleName())
				.include(PerformanceXmlTest.class.getSimpleName())
				.include(PerformancePostgreSqlTest.class.getSimpleName())
				.include(PerformancePostgreSqlJsonTest.class.getSimpleName())
				.include(PerformancePostgreVersioningSqlTest.class.getSimpleName())

				.build();

		new Runner(opt).run();
	}

	@Ignore("Used for testing in IDE")
	@Test
	public void runTransientBenchmarks() throws Exception {
		Options opt = getOptionsBuilder().include(PerformanceTransientTest.class.getSimpleName()).build();
		new Runner(opt).run();
	}

	@Ignore("Used for testing in IDE")
	@Test
	public void runXmlBenchmarks() throws Exception {
		Options opt = getOptionsBuilder().include(PerformanceXmlTest.class.getSimpleName()).build();
		new Runner(opt).run();
	}

	@Ignore("Used for testing in IDE")
	@Test
	public void runPostgreSQLXmlBenchmarks() throws Exception {
		Options opt = getOptionsBuilder().include(PerformancePostgreSqlTest.class.getSimpleName()).build();
		new Runner(opt).run();
	}

	@Ignore("Used for testing in IDE")
	@Test
	public void runPostgreSQLJsonBenchmarks() throws Exception {
		Options opt = getOptionsBuilder().include(PerformancePostgreSqlJsonTest.class.getSimpleName()).build();
		new Runner(opt).run();
	}

	@Ignore("Used for testing in IDE")
	@Test
	public void runPostgreSQLXmlVersioningBenchmarks() throws Exception {
		Options opt = getOptionsBuilder().include(PerformancePostgreVersioningSqlTest.class.getSimpleName()).build();
		new Runner(opt).run();
	}

	private static ChainedOptionsBuilder getOptionsBuilder() {
		return new OptionsBuilder()
				.detectJvmArgs()
				.shouldFailOnError(true)
				.verbosity(VerboseMode.EXTRA)
				.resultFormat(ResultFormatType.JSON)

				.addProfiler(GCProfiler.class)
				.addProfiler(MemPoolProfiler.class)
//				.addProfiler(CompilerProfiler.class)
//				.addProfiler(PausesProfiler.class)
//				.addProfiler(StackProfiler.class)
//				.addProfiler(ClassloaderProfiler.class)

//				.addProfiler(JavaFlightRecorderProfiler.class)

				.forks(1);
	}
}