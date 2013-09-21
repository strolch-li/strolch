/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the ch.eitchnet.java.utils.
 *
 *  ch.eitchnet.java.utils is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  ch.eitchnet.java.utils is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ch.eitchnet.java.utils.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.utils.helper;

import static ch.eitchnet.utils.helper.BaseDecodingTest.PROP_RUN_PERF_TESTS;
import static ch.eitchnet.utils.helper.BaseDecodingTest.isSkipPerfTests;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase16;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32Dmedia;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32Hex;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase64;
import junit.framework.Assert;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class BaseEncodingTest {

	private static final Logger logger = LoggerFactory.getLogger(BaseEncodingTest.class);

	@Test
	public void testBase64() {
		Assert.assertEquals("", toBase64(""));
		Assert.assertEquals("Zg==", toBase64("f"));
		Assert.assertEquals("Zm8=", toBase64("fo"));
		Assert.assertEquals("Zm9v", toBase64("foo"));
		Assert.assertEquals("Zm9vYg==", toBase64("foob"));
		Assert.assertEquals("Zm9vYmE=", toBase64("fooba"));
		Assert.assertEquals("Zm9vYmFy", toBase64("foobar"));
	}

	@Test
	public void testBase32() {
		Assert.assertEquals("", toBase32(""));
		Assert.assertEquals("MY======", toBase32("f"));
		Assert.assertEquals("MZXQ====", toBase32("fo"));
		Assert.assertEquals("MZXW6===", toBase32("foo"));
		Assert.assertEquals("MZXW6YQ=", toBase32("foob"));
		Assert.assertEquals("MZXW6YTB", toBase32("fooba"));
		Assert.assertEquals("MZXW6YTBOI======", toBase32("foobar"));
	}

	@Test
	public void testBase32Hex() {
		Assert.assertEquals("", toBase32Hex(""));
		Assert.assertEquals("CO======", toBase32Hex("f"));
		Assert.assertEquals("CPNG====", toBase32Hex("fo"));
		Assert.assertEquals("CPNMU===", toBase32Hex("foo"));
		Assert.assertEquals("CPNMUOG=", toBase32Hex("foob"));
		Assert.assertEquals("CPNMUOJ1", toBase32Hex("fooba"));
		Assert.assertEquals("CPNMUOJ1E8======", toBase32Hex("foobar"));
	}

	@Test
	public void testBase32Dmedia() {
		Assert.assertEquals("", toBase32Dmedia(""));
		Assert.assertEquals("FCNPVRELI7J9FUUI", toBase32Dmedia("binary foo"));
		Assert.assertEquals("FR======", toBase32Dmedia("f"));
		Assert.assertEquals("FSQJ====", toBase32Dmedia("fo"));
		Assert.assertEquals("FSQPX===", toBase32Dmedia("foo"));
		Assert.assertEquals("FSQPXRJ=", toBase32Dmedia("foob"));
		Assert.assertEquals("FSQPXRM4", toBase32Dmedia("fooba"));
		Assert.assertEquals("FSQPXRM4HB======", toBase32Dmedia("foobar"));
	}

	@Test
	public void testBase16() {
		Assert.assertEquals("", toBase16(""));
		Assert.assertEquals("66", toBase16("f"));
		Assert.assertEquals("666F", toBase16("fo"));
		Assert.assertEquals("666F6F", toBase16("foo"));
		Assert.assertEquals("666F6F62", toBase16("foob"));
		Assert.assertEquals("666F6F6261", toBase16("fooba"));
		Assert.assertEquals("666F6F626172", toBase16("foobar"));
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
