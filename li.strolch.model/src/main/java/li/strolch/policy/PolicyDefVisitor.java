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
package li.strolch.policy;

/**
 * This is the visitor interface to resolve a policy from a {@link PolicyDef}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PolicyDefVisitor {

	/**
	 * Resolves a policy instance by returning an instance of a Java class by instantiating it by the
	 * {@link PolicyDef#getValue()} defined on the {@link JavaPolicyDef}
	 * 
	 * @param javaPolicyDef
	 *            the {@link PolicyDef} referencing a Java Class
	 * 
	 * @return an instance of the policy referenced by the {@link PolicyDef#getValue()}
	 */
	public <T> T visit(JavaPolicyDef javaPolicyDef);

	/**
	 * This method resolves a Policy by further indirection. I.e. the {@link PolicyDef#getValue()} is a key to the
	 * actual implementation. Use this if many elements use the same policy and it is required to change the
	 * implementation easily
	 * 
	 * @param keyPolicyDef
	 *            the {@link PolicyDef} using a key to reference a policy
	 * 
	 * @return an instance of the policy resolved by the key {@link PolicyDef#getValue()}
	 */
	public <T> T visit(KeyPolicyDef keyPolicyDef);
}