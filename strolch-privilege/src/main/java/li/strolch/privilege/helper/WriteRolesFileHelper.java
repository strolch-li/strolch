package li.strolch.privilege.helper;

import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.xml.PrivilegeRolesSaxReader;
import li.strolch.privilege.xml.PrivilegeRolesSaxWriter;
import li.strolch.utils.helper.XmlHelper;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class WriteRolesFileHelper {

	public static void main(String[] args) throws XMLStreamException, IOException {

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

		PrivilegeRolesSaxWriter configSaxWriter = new PrivilegeRolesSaxWriter(roles, dst);
		configSaxWriter.write();
	}
}
