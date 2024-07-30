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

/**
 * This {@link PolicyDef} adds a further indirection in the resolving of a policy. In this {@link PolicyDef} the value
 * is a key used to look up the concrete implementation. Where the look-up is performed depends on the {@link
 * PolicyDefVisitor}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class KeyPolicyDef extends PolicyDef {

	public static final String XML_PREFIX = "key:";

	public KeyPolicyDef(String type, String value) {
		super(type, value);
	}

	@Override
	public <T> Class<T> accept(PolicyDefVisitor visitor) throws ClassNotFoundException {
		return visitor.visit(this);
	}

	@Override
	public String getValueForXml() {
		return XML_PREFIX + this.value;
	}

	@Override
	public KeyPolicyDef getClone() {
		return new KeyPolicyDef(this.type, this.value);
	}
}