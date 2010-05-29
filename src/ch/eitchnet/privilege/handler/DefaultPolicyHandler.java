/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.handler;

import java.io.File;
import java.util.Map;

import org.dom4j.Element;

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.base.XmlConstants;
import ch.eitchnet.privilege.helper.ConfigurationHelper;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.RestrictionPolicy;
import ch.eitchnet.privilege.model.internal.Role;

/**
 * @author rvonburg
 * 
 */
public class DefaultPolicyHandler implements PolicyHandler {

	private Map<String, RestrictionPolicy> policyMap;

	/**
	 * @see ch.eitchnet.privilege.handler.PolicyHandler#actionAllowed(ch.eitchnet.privilege.model.internal.Role,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(Role role, Restrictable restrictable) {

		// user and restrictable must not be null
		if (role == null)
			throw new PrivilegeException("Role may not be null!");
		else if (restrictable == null)
			throw new PrivilegeException("Restrictable may not be null!");

		// validate restriction key for this restrictable
		String restrictionKey = restrictable.getRestrictionKey();
		if (restrictionKey == null || restrictionKey.length() < 3) {
			throw new PrivilegeException(
					"The RestrictionKey may not be shorter than 3 characters. Invalid Restrictable "
							+ restrictable.getClass().getName());
		}

		// get restriction policy
		RestrictionPolicy policy = policyMap.get(restrictionKey);
		if (policy == null) {
			throw new PrivilegeException("No RestrictionPolicy exists for the RestrictionKey " + restrictionKey
					+ " for Restrictable " + restrictable.getClass().getName());
		}

		// delegate checking to restriction policy
		return policy.actionAllowed(role, restrictable);
	}

	/**
	 * @see ch.eitchnet.privilege.base.PrivilegeContainerObject#initialize(org.dom4j.Element)
	 */
	public void initialize(Element element) {

		// get parameters
		Element parameterElement = element.element(XmlConstants.XML_PARAMETERS);
		Map<String, String> parameterMap = ConfigurationHelper.convertToParameterMap(parameterElement);

		// get policy file name
		String policyFileName = parameterMap.get(XmlConstants.XML_PARAM_POLICY_FILE);
		if (policyFileName == null || policyFileName.isEmpty()) {
			throw new PrivilegeException("[" + PolicyHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_POLICY_FILE + " is invalid");
		}

		// get policy file
		File policyFile = new File(PrivilegeContainer.getInstance().getBasePath() + "/" + policyFileName);
		if (!policyFile.exists()) {
			throw new PrivilegeException("[" + PolicyHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_POLICY_FILE + " is invalid as policy file does not exist at path "
					+ policyFile.getAbsolutePath());
		}

		// parse policy xml file to XML document
		Element containerRootElement = XmlHelper.parseDocument(policyFile).getRootElement();
	}
}
