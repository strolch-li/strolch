/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
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
 * TODO: Note: currently the password input is echoed which is a security risk. This is a TODO
 * </p>
 * 
 * @author rvonburg
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
