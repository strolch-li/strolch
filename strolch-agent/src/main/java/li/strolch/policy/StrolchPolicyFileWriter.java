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

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.policy.StrolchPolicyFileParser.PolicyModel;
import li.strolch.policy.StrolchPolicyFileParser.PolicyType;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import static li.strolch.policy.PolicySerializationConstants.*;
import static li.strolch.privilege.helper.XmlHelper.openXmlStreamWriterDocument;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchPolicyFileWriter {

	private final File policyFile;

	public StrolchPolicyFileWriter(File policyFile) {
		this.policyFile = policyFile;
	}

	public void save(PolicyModel policyModel) {
		try (OutputStream out = Files.newOutputStream(this.policyFile.toPath());
		     Writer ioWriter = new OutputStreamWriter(out, StandardCharsets.UTF_8)) {

			IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
			xmlWriter.writeStartElement(STROLCH_POLICIES);

			Map<String, PolicyType> policyTypes = policyModel.getPolicyTypes();
			List<String> policyTypeNames = new ArrayList<>(policyTypes.keySet());
			policyTypeNames.sort(Comparator.naturalOrder());
			for (String policyTypeName : policyTypeNames) {
				PolicyType policyType = policyTypes.get(policyTypeName);
				writePolicyType(xmlWriter, policyType);
			}

			xmlWriter.writeEndDocument();
			xmlWriter.flush();

		} catch (Exception e) {
			throw new RuntimeException("Failed to save policies to " + this.policyFile.getAbsolutePath(), e);
		}
	}

	private void writePolicyType(IndentingXMLStreamWriter xmlWriter, PolicyType policyType) throws XMLStreamException {
		Map<String, String> policyByKeyMap = policyType.getPolicyByKeyMap();
		if (policyByKeyMap.isEmpty())
			xmlWriter.writeEmptyElement(POLICY_TYPE);
		else
			xmlWriter.writeStartElement(POLICY_TYPE);
		xmlWriter.writeAttribute(TYPE, policyType.getType());
		xmlWriter.writeAttribute(API, policyType.getApi());

		for (Map.Entry<String, String> entry : policyByKeyMap.entrySet()) {
			xmlWriter.writeEmptyElement(POLICY);
			xmlWriter.writeAttribute(KEY, entry.getKey());
			xmlWriter.writeAttribute(CLASS, entry.getValue());
		}

		if (!policyByKeyMap.isEmpty())
			xmlWriter.writeEndElement();
	}
}
