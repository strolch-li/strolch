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
 * @author rvonburg
 * 
 */
public class PasswordCreator {

	/**
	 * @param args
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
		System.out.print("Hash is: " + EncryptionHelper.encryptString(hashAlgorithm, password));
	}

}
