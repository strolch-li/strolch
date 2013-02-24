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

import static ch.eitchnet.utils.helper.BaseDecoding.fromBase16;
import static ch.eitchnet.utils.helper.BaseDecoding.fromBase32;
import static ch.eitchnet.utils.helper.BaseDecoding.fromBase32Hex;
import static ch.eitchnet.utils.helper.BaseDecoding.fromBase64;
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
		Assert.assertEquals("", new String(fromBase64("".getBytes())));
		Assert.assertEquals("f", new String(fromBase64("Zg==".getBytes())));
		Assert.assertEquals("fo", new String(fromBase64("Zm8=".getBytes())));
		Assert.assertEquals("foo", new String(fromBase64("Zm9v".getBytes())));
		Assert.assertEquals("foob", new String(fromBase64("Zm9vYg==".getBytes())));
		Assert.assertEquals("fooba", new String(fromBase64("Zm9vYmE=".getBytes())));
		Assert.assertEquals("foobar", new String(fromBase64("Zm9vYmFy".getBytes())));

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
		Assert.assertEquals("", new String(fromBase32("".getBytes())));
		Assert.assertEquals("f", new String(fromBase32("MY======".getBytes())));
		Assert.assertEquals("fo", new String(fromBase32("MZXQ====".getBytes())));
		Assert.assertEquals("foo", new String(fromBase32("MZXW6===".getBytes())));
		Assert.assertEquals("foob", new String(fromBase32("MZXW6YQ=".getBytes())));
		Assert.assertEquals("fooba", new String(fromBase32("MZXW6YTB".getBytes())));
		Assert.assertEquals("foobar", new String(fromBase32("MZXW6YTBOI======".getBytes())));

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
		Assert.assertEquals("", new String(fromBase32Hex("".getBytes())));
		Assert.assertEquals("f", new String(fromBase32Hex("CO======".getBytes())));
		Assert.assertEquals("fo", new String(fromBase32Hex("CPNG====".getBytes())));
		Assert.assertEquals("foo", new String(fromBase32Hex("CPNMU===".getBytes())));
		Assert.assertEquals("foob", new String(fromBase32Hex("CPNMUOG=".getBytes())));
		Assert.assertEquals("fooba", new String(fromBase32Hex("CPNMUOJ1".getBytes())));
		Assert.assertEquals("foobar", new String(fromBase32Hex("CPNMUOJ1E8======".getBytes())));

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
	public void testBase16() {
		Assert.assertEquals("", new String(fromBase16("".getBytes())));
		Assert.assertEquals("f", new String(fromBase16("66".getBytes())));
		Assert.assertEquals("fo", new String(fromBase16("666F".getBytes())));
		Assert.assertEquals("foo", new String(fromBase16("666F6F".getBytes())));
		Assert.assertEquals("foob", new String(fromBase16("666F6F62".getBytes())));
		Assert.assertEquals("fooba", new String(fromBase16("666F6F6261".getBytes())));
		Assert.assertEquals("foobar", new String(fromBase16("666F6F626172".getBytes())));

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
