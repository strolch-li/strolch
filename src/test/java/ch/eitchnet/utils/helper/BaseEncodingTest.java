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

import static ch.eitchnet.utils.helper.BaseEncoding.toBase16;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32Hex;
import static ch.eitchnet.utils.helper.BaseEncoding.toBase32Dmedia;
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
		Assert.assertEquals("", new String(toBase64("".getBytes())));
		Assert.assertEquals("Zg==", new String(toBase64("f".getBytes())));
		Assert.assertEquals("Zm8=", new String(toBase64("fo".getBytes())));
		Assert.assertEquals("Zm9v", new String(toBase64("foo".getBytes())));
		Assert.assertEquals("Zm9vYg==", new String(toBase64("foob".getBytes())));
		Assert.assertEquals("Zm9vYmE=", new String(toBase64("fooba".getBytes())));
		Assert.assertEquals("Zm9vYmFy", new String(toBase64("foobar".getBytes())));

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase64(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base64 took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase32() {
		Assert.assertEquals("", new String(toBase32("".getBytes())));
		Assert.assertEquals("MY======", new String(toBase32("f".getBytes())));
		Assert.assertEquals("MZXQ====", new String(toBase32("fo".getBytes())));
		Assert.assertEquals("MZXW6===", new String(toBase32("foo".getBytes())));
		Assert.assertEquals("MZXW6YQ=", new String(toBase32("foob".getBytes())));
		Assert.assertEquals("MZXW6YTB", new String(toBase32("fooba".getBytes())));
		Assert.assertEquals("MZXW6YTBOI======", new String(toBase32("foobar".getBytes())));

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase32(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base32 took " + StringHelper.formatNanoDuration(end - start));
	}

	@Test
	public void testBase32Hex() {
		Assert.assertEquals("", new String(toBase32Hex("".getBytes())));
		Assert.assertEquals("CO======", new String(toBase32Hex("f".getBytes())));
		Assert.assertEquals("CPNG====", new String(toBase32Hex("fo".getBytes())));
		Assert.assertEquals("CPNMU===", new String(toBase32Hex("foo".getBytes())));
		Assert.assertEquals("CPNMUOG=", new String(toBase32Hex("foob".getBytes())));
		Assert.assertEquals("CPNMUOJ1", new String(toBase32Hex("fooba".getBytes())));
		Assert.assertEquals("CPNMUOJ1E8======", new String(toBase32Hex("foobar".getBytes())));

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase32Hex(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base32Hex took " + StringHelper.formatNanoDuration(end - start));
	}
	
	@Test
	public void testBase32Dmedia() {
		
		Assert.assertEquals("", new String(toBase32Dmedia("".getBytes())));
		Assert.assertEquals("FCNPVRELI7J9FUUI", new String(toBase32Dmedia("binary foo".getBytes())));
		Assert.assertEquals("FR======", new String(toBase32Dmedia("f".getBytes())));
		Assert.assertEquals("FSQJ====", new String(toBase32Dmedia("fo".getBytes())));
		Assert.assertEquals("FSQPX===", new String(toBase32Dmedia("foo".getBytes())));
		Assert.assertEquals("FSQPXRJ=", new String(toBase32Dmedia("foob".getBytes())));
		Assert.assertEquals("FSQPXRM4", new String(toBase32Dmedia("fooba".getBytes())));
		Assert.assertEquals("FSQPXRM4HB======", new String(toBase32Dmedia("foobar".getBytes())));

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
		Assert.assertEquals("", new String(toBase16("".getBytes())));
		Assert.assertEquals("66", new String(toBase16("f".getBytes())));
		Assert.assertEquals("666F", new String(toBase16("fo".getBytes())));
		Assert.assertEquals("666F6F", new String(toBase16("foo".getBytes())));
		Assert.assertEquals("666F6F62", new String(toBase16("foob".getBytes())));
		Assert.assertEquals("666F6F6261", new String(toBase16("fooba".getBytes())));
		Assert.assertEquals("666F6F626172", new String(toBase16("foobar".getBytes())));

		long start = System.nanoTime();
		byte[] bytes = new byte[1024 * 1024];
		for (int i = 0; i < 200; i++) {
			toBase16(bytes);
		}
		long end = System.nanoTime();
		logger.info("Encoding 200MB Base16 took " + StringHelper.formatNanoDuration(end - start));
	}
}
