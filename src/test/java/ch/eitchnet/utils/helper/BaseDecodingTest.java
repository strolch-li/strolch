/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.utils.helper;

import static ch.eitchnet.utils.helper.BaseEncoding.fromBase16;
import static ch.eitchnet.utils.helper.BaseEncoding.fromBase32;
import static ch.eitchnet.utils.helper.BaseEncoding.fromBase32Dmedia;
import static ch.eitchnet.utils.helper.BaseEncoding.fromBase32Hex;
import static ch.eitchnet.utils.helper.BaseEncoding.fromBase64;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32Hex;
import junit.framework.Assert;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class BaseDecodingTest {
	private static final Logger logger = LoggerFactory.getLogger(BaseDecodingTest.class);

	@Test
	public void testBase64() {
		Assert.assertEquals("", fromBase64(""));
		Assert.assertEquals("f", fromBase64("Zg=="));
		Assert.assertEquals("fo", fromBase64("Zm8="));
		Assert.assertEquals("foo", fromBase64("Zm9v"));
		Assert.assertEquals("foob", fromBase64("Zm9vYg=="));
		Assert.assertEquals("fooba", fromBase64("Zm9vYmE="));
		Assert.assertEquals("foobar", fromBase64("Zm9vYmFy"));

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
	public void testBase32() {
		Assert.assertEquals("", fromBase32(""));
		Assert.assertEquals("f", fromBase32("MY======"));
		Assert.assertEquals("fo", fromBase32("MZXQ===="));
		Assert.assertEquals("foo", fromBase32("MZXW6==="));
		Assert.assertEquals("foob", fromBase32("MZXW6YQ="));
		Assert.assertEquals("fooba", fromBase32("MZXW6YTB"));
		Assert.assertEquals("foobar", fromBase32("MZXW6YTBOI======"));

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
	public void testBase32Hex() {
		Assert.assertEquals("", fromBase32Hex(""));
		Assert.assertEquals("f", fromBase32Hex("CO======"));
		Assert.assertEquals("fo", fromBase32Hex("CPNG===="));
		Assert.assertEquals("foo", fromBase32Hex("CPNMU==="));
		Assert.assertEquals("foob", fromBase32Hex("CPNMUOG="));
		Assert.assertEquals("fooba", fromBase32Hex("CPNMUOJ1"));
		Assert.assertEquals("foobar", fromBase32Hex("CPNMUOJ1E8======"));

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
	public void testBase32Dmedia() {

		Assert.assertEquals("", fromBase32Dmedia(""));
		Assert.assertEquals("binary foo", fromBase32Dmedia("FCNPVRELI7J9FUUI"));
		Assert.assertEquals("f", fromBase32Dmedia("FR======"));
		Assert.assertEquals("fo", fromBase32Dmedia("FSQJ===="));
		Assert.assertEquals("foo", fromBase32Dmedia("FSQPX==="));
		Assert.assertEquals("foob", fromBase32Dmedia("FSQPXRJ="));
		Assert.assertEquals("fooba", fromBase32Dmedia("FSQPXRM4"));
		Assert.assertEquals("foobar", fromBase32Dmedia("FSQPXRM4HB======"));

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase32Hex(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base32Dmedia took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase16() {
		Assert.assertEquals("", fromBase16(""));
		Assert.assertEquals("f", fromBase16("66"));
		Assert.assertEquals("fo", fromBase16("666F"));
		Assert.assertEquals("foo", fromBase16("666F6F"));
		Assert.assertEquals("foob", fromBase16("666F6F62"));
		Assert.assertEquals("fooba", fromBase16("666F6F6261"));
		Assert.assertEquals("foobar", fromBase16("666F6F626172"));

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
