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
package li.strolch.utils.helper;

import static li.strolch.utils.helper.BaseEncoding.fromBase16;
import static li.strolch.utils.helper.BaseEncoding.fromBase32;
import static li.strolch.utils.helper.BaseEncoding.fromBase32Dmedia;
import static li.strolch.utils.helper.BaseEncoding.fromBase32Hex;
import static li.strolch.utils.helper.BaseEncoding.fromBase64;
import static li.strolch.utils.helper.BaseEncoding.toBase32Hex;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.utils.helper.PropertiesHelper;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class BaseDecodingTest {
	public static final String PROP_RUN_PERF_TESTS = "li.strolch.utils.test.runPerfTests"; //$NON-NLS-1$
	private static final Logger logger = LoggerFactory.getLogger(BaseDecodingTest.class);

	public static boolean isSkipPerfTests() {
		String context = BaseDecodingTest.class.getSimpleName();
		String key = PROP_RUN_PERF_TESTS;
		boolean runPerfTests = PropertiesHelper.getPropertyBool(context, key, Boolean.FALSE);
		return !runPerfTests;
	}

	@Test
	public void testBase64() {
		assertEquals("", fromBase64(""));
		assertEquals("f", fromBase64("Zg=="));
		assertEquals("fo", fromBase64("Zm8="));
		assertEquals("foo", fromBase64("Zm9v"));
		assertEquals("foob", fromBase64("Zm9vYg=="));
		assertEquals("fooba", fromBase64("Zm9vYmE="));
		assertEquals("foobar", fromBase64("Zm9vYmFy"));
	}

	@Test
	public void testBase32() {
		assertEquals("", fromBase32(""));
		assertEquals("f", fromBase32("MY======"));
		assertEquals("fo", fromBase32("MZXQ===="));
		assertEquals("foo", fromBase32("MZXW6==="));
		assertEquals("foob", fromBase32("MZXW6YQ="));
		assertEquals("fooba", fromBase32("MZXW6YTB"));
		assertEquals("foobar", fromBase32("MZXW6YTBOI======"));
	}

	@Test
	public void testBase32Hex() {
		assertEquals("", fromBase32Hex(""));
		assertEquals("f", fromBase32Hex("CO======"));
		assertEquals("fo", fromBase32Hex("CPNG===="));
		assertEquals("foo", fromBase32Hex("CPNMU==="));
		assertEquals("foob", fromBase32Hex("CPNMUOG="));
		assertEquals("fooba", fromBase32Hex("CPNMUOJ1"));
		assertEquals("foobar", fromBase32Hex("CPNMUOJ1E8======"));
	}

	@Test
	public void testBase32Dmedia() {

		assertEquals("", fromBase32Dmedia(""));
		assertEquals("binary foo", fromBase32Dmedia("FCNPVRELI7J9FUUI"));
		assertEquals("f", fromBase32Dmedia("FR======"));
		assertEquals("fo", fromBase32Dmedia("FSQJ===="));
		assertEquals("foo", fromBase32Dmedia("FSQPX==="));
		assertEquals("foob", fromBase32Dmedia("FSQPXRJ="));
		assertEquals("fooba", fromBase32Dmedia("FSQPXRM4"));
		assertEquals("foobar", fromBase32Dmedia("FSQPXRM4HB======"));
	}

	@Test
	public void testBase16() {
		assertEquals("", fromBase16(""));
		assertEquals("f", fromBase16("66"));
		assertEquals("fo", fromBase16("666F"));
		assertEquals("foo", fromBase16("666F6F"));
		assertEquals("foob", fromBase16("666F6F62"));
		assertEquals("fooba", fromBase16("666F6F6261"));
		assertEquals("foobar", fromBase16("666F6F626172"));
	}

	@Test
	public void testBase64Perf() {
		if (isSkipPerfTests()) {
			return;
		}

		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < bytes.length; i++) {
			bytes[i] = 'Z';
		}
		long start = System.nanoTime();
		for (int i = 0; i < 200; i++) {
			fromBase64(bytes);
		}
		long end = System.nanoTime();
		logger.info("Decoding 200MB Base64 took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase32Perf() {
		if (isSkipPerfTests()) {
			logger.info("Not running performance tests as not enabled by system property " + PROP_RUN_PERF_TESTS);
			return;
		}

		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < bytes.length; i++) {
			bytes[i] = 'M';
		}
		long start = System.nanoTime();
		for (int i = 0; i < 200; i++) {
			fromBase32(bytes);
		}
		long end = System.nanoTime();
		logger.info("Decoding 200MB Base32 took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase32HexPerf() {
		if (isSkipPerfTests()) {
			logger.info("Not running performance tests as not enabled by system property " + PROP_RUN_PERF_TESTS);
			return;
		}

		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < bytes.length; i++) {
			bytes[i] = 'C';
		}
		long start = System.nanoTime();
		for (int i = 0; i < 200; i++) {
			fromBase32Hex(bytes);
		}
		long end = System.nanoTime();
		logger.info("Decoding 200MB Base32Hex took " + StringHelper.formatNanoDuration(end - start));
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

		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < bytes.length; i++) {
			bytes[i] = '6';
		}
		long start = System.nanoTime();
		for (int i = 0; i < 200; i++) {
			fromBase16(bytes);
		}
		long end = System.nanoTime();
		logger.info("Decoding 200MB Base16 took " + StringHelper.formatNanoDuration(end - start));
	}
}
