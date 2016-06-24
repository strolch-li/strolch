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
package ch.eitchnet.privilege.helper;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.security.MessageDigest;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * <p>
 * Simple main class which can be used to create a hash from a password which the user must type in at the command line
 * </p>
 * 
 * <p>
 * TODO: Note: currently the password input is echoed which is a security risk
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PasswordCreator {

	/**
	 * @param args
	 *            the args from the command line, NOT USED
	 * @throws Exception
	 *             thrown if anything goes wrong
	 */
	@SuppressWarnings("nls")
	public static void main(String[] args) throws Exception {

		BufferedReader r = new BufferedReader(new InputStreamReader(System.in));

		String hashAlgorithm = null;
		while (hashAlgorithm == null) {
			System.out.print("Hash Algorithm [SHA-256]: ");
			String readLine = r.readLine().trim();

			if (readLine.isEmpty()) {
				hashAlgorithm = "SHA-256";
			} else {

				try {
					MessageDigest.getInstance(readLine);
					hashAlgorithm = readLine;
				} catch (Exception e) {
					System.out.println(e.getLocalizedMessage());
					hashAlgorithm = null;
				}
			}
		}

		System.out.print("Password: ");
		String password = r.readLine().trim();
		System.out.print("Hash is: " + StringHelper.hashAsHex(hashAlgorithm, password));
	}

}
