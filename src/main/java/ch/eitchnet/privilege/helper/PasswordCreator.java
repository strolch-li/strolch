/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.privilege.helper;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.security.MessageDigest;

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
		System.out.print("Hash is: " + HashHelper.stringToHash(hashAlgorithm, password));
	}

}
