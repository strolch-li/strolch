package li.strolch.privilege.helper;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.xml.PrivilegeRolesDomWriter;
import li.strolch.privilege.xml.PrivilegeRolesSaxReader;
import li.strolch.utils.helper.XmlHelper;

public class WriteRolesFileHelper {

	public static void main(String[] args) {

		if (args.length != 2)
			throw new IllegalStateException("Usage: <src> <dst>");

		File src = new File(args[0]);
		File dst = new File(args[1]);

		if (!src.exists())
			throw new IllegalStateException("Source file " + src + " does not exist!");
		if (dst.exists())
			throw new IllegalStateException("Destination file " + src + " exists already!");

		PrivilegeRolesSaxReader xmlHandler = new PrivilegeRolesSaxReader();
		XmlHelper.parseDocument(src, xmlHandler);

		Map<String, Role> rolesMap = xmlHandler.getRoles();
		List<Role> roles = new ArrayList<>(rolesMap.values());

		PrivilegeRolesDomWriter configSaxWriter = new PrivilegeRolesDomWriter(roles, dst);
		configSaxWriter.write();
	}
}
