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
package ch.eitchnet.utils.helper;

import static ch.eitchnet.utils.helper.BaseDecodingTest.PROP_RUN_PERF_TESTS;
import static ch.eitchnet.utils.helper.BaseDecodingTest.isSkipPerfTests;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase16;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32Dmedia;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32Hex;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase64;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class BaseEncodingTest {

	private static final Logger logger = LoggerFactory.getLogger(BaseEncodingTest.class);

	@Test
	public void testBase64() {
		assertEquals("", toBase64(""));
		assertEquals("Zg==", toBase64("f"));
		assertEquals("Zm8=", toBase64("fo"));
		assertEquals("Zm9v", toBase64("foo"));
		assertEquals("Zm9vYg==", toBase64("foob"));
		assertEquals("Zm9vYmE=", toBase64("fooba"));
		assertEquals("Zm9vYmFy", toBase64("foobar"));
	}

	@Test
	public void testBase32() {
		assertEquals("", toBase32(""));
		assertEquals("MY======", toBase32("f"));
		assertEquals("MZXQ====", toBase32("fo"));
		assertEquals("MZXW6===", toBase32("foo"));
		assertEquals("MZXW6YQ=", toBase32("foob"));
		assertEquals("MZXW6YTB", toBase32("fooba"));
		assertEquals("MZXW6YTBOI======", toBase32("foobar"));
	}

	@Test
	public void testBase32Hex() {
		assertEquals("", toBase32Hex(""));
		assertEquals("CO======", toBase32Hex("f"));
		assertEquals("CPNG====", toBase32Hex("fo"));
		assertEquals("CPNMU===", toBase32Hex("foo"));
		assertEquals("CPNMUOG=", toBase32Hex("foob"));
		assertEquals("CPNMUOJ1", toBase32Hex("fooba"));
		assertEquals("CPNMUOJ1E8======", toBase32Hex("foobar"));
	}

	@Test
	public void testBase32Dmedia() {
		assertEquals("", toBase32Dmedia(""));
		assertEquals("FCNPVRELI7J9FUUI", toBase32Dmedia("binary foo"));
		assertEquals("FR======", toBase32Dmedia("f"));
		assertEquals("FSQJ====", toBase32Dmedia("fo"));
		assertEquals("FSQPX===", toBase32Dmedia("foo"));
		assertEquals("FSQPXRJ=", toBase32Dmedia("foob"));
		assertEquals("FSQPXRM4", toBase32Dmedia("fooba"));
		assertEquals("FSQPXRM4HB======", toBase32Dmedia("foobar"));
	}

	@Test
	public void testBase16() {
		assertEquals("", toBase16(""));
		assertEquals("66", toBase16("f"));
		assertEquals("666F", toBase16("fo"));
		assertEquals("666F6F", toBase16("foo"));
		assertEquals("666F6F62", toBase16("foob"));
		assertEquals("666F6F6261", toBase16("fooba"));
		assertEquals("666F6F626172", toBase16("foobar"));
	}

	@Test
	public void testBase64Perf() {
		if (isSkipPerfTests()) {
			logger.info("Not running performance tests as not enabled by system property " + PROP_RUN_PERF_TESTS);
			return;
		}

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase64(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base64 took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase32Perf() {
		if (isSkipPerfTests()) {
			logger.info("Not running performance tests as not enabled by system property " + PROP_RUN_PERF_TESTS);
			return;
		}

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase32(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base32 took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase32HexPerf() {
		if (isSkipPerfTests()) {
			logger.info("Not running performance tests as not enabled by system property " + PROP_RUN_PERF_TESTS);
			return;
		}

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase32Hex(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base32Hex took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase32DmediaPerf() {
		if (isSkipPerfTests()) {
			logger.info("Not running performance tests as not enabled by system property " + PROP_RUN_PERF_TESTS);
			return;
		}

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase32Hex(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base32Dmedia took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase16Perf() {
		if (isSkipPerfTests()) {
			logger.info("Not running performance tests as not enabled by system property " + PROP_RUN_PERF_TESTS);
			return;
		}

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase16(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base16 took " + StringHelper.formatNanoDuration(end - start));
	}
}
