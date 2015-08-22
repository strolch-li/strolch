/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.policy;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PolicyDefTest {

	@Test
	public void shouldInstantiateJavaPolicyDef() {
		PolicyDef def = PolicyDef.valueOf("MyJavaType", JavaPolicyDef.XML_PREFIX + "java.lang.Object");
		assertEquals(JavaPolicyDef.class, def.getClass());
		assertEquals("MyJavaType", def.getType());
		assertEquals("java.lang.Object", def.getValue());
		assertEquals(JavaPolicyDef.XML_PREFIX + "java.lang.Object", def.getValueForXml());
	}

	@Test
	public void shouldInstantiateKeyPolicyDef() {
		PolicyDef def = PolicyDef.valueOf("MyKeyType", KeyPolicyDef.XML_PREFIX + "MyKey");
		assertEquals(KeyPolicyDef.class, def.getClass());
		assertEquals("MyKeyType", def.getType());
		assertEquals("MyKey", def.getValue());
		assertEquals(KeyPolicyDef.XML_PREFIX + "MyKey", def.getValueForXml());
	}

	@Test
	public void shouldClonePolicyDefs() {

		PolicyDef javaDef = PolicyDef.valueOf("MyJavaType", JavaPolicyDef.XML_PREFIX + "java.lang.Object");
		PolicyDef keyDef = PolicyDef.valueOf("MyKeyType", KeyPolicyDef.XML_PREFIX + "MyKey");

		PolicyDefs defs = new PolicyDefs();
		defs.addOrUpdate(javaDef);
		defs.addOrUpdate(keyDef);

		assertEquals(2, defs.getPolicyTypes().size());

		PolicyDefs clone = defs.getClone();
		assertEquals(2, clone.getPolicyTypes().size());

		PolicyDef javaClone = clone.getPolicyDef("MyJavaType");
		assertEquals(JavaPolicyDef.class, javaClone.getClass());
		assertEquals("MyJavaType", javaClone.getType());
		assertEquals("java.lang.Object", javaClone.getValue());
		assertEquals(JavaPolicyDef.XML_PREFIX + "java.lang.Object", javaClone.getValueForXml());

		PolicyDef keyClone = clone.getPolicyDef("MyKeyType");
		assertEquals(KeyPolicyDef.class, keyClone.getClass());
		assertEquals("MyKeyType", keyClone.getType());
		assertEquals("MyKey", keyClone.getValue());
		assertEquals(KeyPolicyDef.XML_PREFIX + "MyKey", keyClone.getValueForXml());
	}
}
