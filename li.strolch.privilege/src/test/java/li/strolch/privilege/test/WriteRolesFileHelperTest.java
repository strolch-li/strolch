package li.strolch.privilege.test;

import static li.strolch.privilege.test.XmlTest.SRC_TEST;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import li.strolch.privilege.helper.WriteRolesFileHelper;
import org.junit.Test;

public class WriteRolesFileHelperTest {

	@Test
	public void shouldReadAndWriteRolesFile() {

		String src = SRC_TEST + "PrivilegeRoles.xml";
		String dst = "target/WriteRolesFileHelperTest_roles.xml";

		assertFalse(new File(dst).exists());

		WriteRolesFileHelper.main(new String[] { src, dst });

		assertTrue(new File(dst).exists());
	}
}
