/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.policy.StrolchPolicyFileParser.PolicyModel;
import li.strolch.policy.StrolchPolicyFileParser.PolicyType;
import li.strolch.utils.helper.XmlHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.util.Map;

import static li.strolch.policy.PolicySerializationConstants.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchPolicyFileWriter {

	private final File policyFile;

	public StrolchPolicyFileWriter(File policyFile) {
		this.policyFile = policyFile;
	}

	public void save(PolicyModel policyModel) {
		try {
			Document doc = XmlHelper.getDocumentBuilder().newDocument();
			Element rootElement = doc.createElement(STROLCH_POLICIES);
			doc.appendChild(rootElement);

			for (PolicyType policyType : policyModel.getPolicyTypes().values()) {
				Element policyTypeE = doc.createElement(POLICY_TYPE);
				policyTypeE.setAttribute(TYPE, policyType.getType());
				policyTypeE.setAttribute(API, policyType.getApi());
				rootElement.appendChild(policyTypeE);

				for (Map.Entry<String, String> entry : policyType.getPolicyByKeyMap().entrySet()) {
					Element policyE = doc.createElement(POLICY);
					policyE.setAttribute(KEY, entry.getKey());
					policyE.setAttribute(CLASS, entry.getValue());
					policyTypeE.appendChild(policyE);
				}
			}

			XmlHelper.writeDocument(doc, this.policyFile);

		} catch (Exception e) {
			throw new RuntimeException("Failed to save policies to " + this.policyFile.getAbsolutePath(), e);
		}
	}
}
