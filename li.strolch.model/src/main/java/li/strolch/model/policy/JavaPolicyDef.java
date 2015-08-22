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
 * Simple {@link PolicyDef} where the value references a concrete class which can simply be instantiated
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class JavaPolicyDef extends PolicyDef {

	public static final String XML_PREFIX = "java:";

	public JavaPolicyDef(String type, String value) {
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
	public JavaPolicyDef getClone() {
		return new JavaPolicyDef(this.type, this.value);
	}
}