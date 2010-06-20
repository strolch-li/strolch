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

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.internal.User;

/**
 * @author rvonburg
 * 
 */
public class PrivilegeHelper {

	public static boolean isUserPrivilegeAdmin(Certificate certificate) {
		// validate certificate
		if (!PrivilegeContainer.getInstance().getSessionHandler().isCertificateValid(certificate)) {
			throw new PrivilegeException("Certificate " + certificate + " is not valid!");
		}

		// get user object
		User user = PrivilegeContainer.getInstance().getModelHandler().getUser(certificate.getUsername());
		if (user == null) {
			throw new PrivilegeException(
					"Oh boy, how did this happen: No User in user map although the certificate is valid!");
		}

		// validate user has PrivilegeAdmin role
		if (!user.hasRole(PrivilegeContainer.PRIVILEGE_ADMIN_ROLE)) {
			throw new AccessDeniedException("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role!");
		} else {
			return true;
		}
	}
}
