/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.command;

import static ch.eitchnet.utils.helper.StringHelper.UNDERLINE;

import java.io.File;
import java.io.FileOutputStream;
import java.text.MessageFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.OrderVisitor;
import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.Tags;
import li.strolch.model.xml.OrderToSaxWriterVisitor;
import li.strolch.model.xml.ResourceToSaxWriterVisitor;
import li.strolch.model.xml.XmlModelSaxReader.XmlModelStatistics;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.api.Command;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlExportModelCommand extends Command {

	public static final String XML_FILE_SUFFIX = ".xml";

	// input
	private File modelFile;
	private boolean doOrders;
	private boolean doResources;
	private Set<String> orderTypes;
	private Set<String> resourceTypes;

	// output
	private XmlModelStatistics statistics;
	private boolean multiFile;

	public XmlExportModelCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void doCommand() {
		DBC.PRE.assertNotExists("Model may not already exist!", this.modelFile);
		String fileName = this.modelFile.getName();
		DBC.PRE.assertTrue("Model file must end with .xml", fileName.endsWith(XML_FILE_SUFFIX));

		long start = System.nanoTime();
		this.statistics = new XmlModelStatistics();
		this.statistics.startTime = new Date();

		String exportName = fileName.substring(0, fileName.indexOf(XML_FILE_SUFFIX));
		Set<File> createdFiles = new HashSet<>();

		try (FileOutputStream out = new FileOutputStream(this.modelFile)) {
			createdFiles.add(this.modelFile);

			XMLStreamWriter writer = openXmlStreamWriter(out);

			if (this.doResources) {
				ResourceMap resourceMap = tx().getResourceMap();
				Set<String> resourceTypesToExport = resourceMap.getTypes(tx());
				if (!this.resourceTypes.isEmpty())
					resourceTypesToExport.retainAll(this.resourceTypes);

				resourceTypesToExport = new TreeSet<>(resourceTypesToExport);
				for (String type : resourceTypesToExport) {

					if (!multiFile) {
						writeResourcesByType(writer, resourceMap, type);
					} else {
						String typeXmlFile = exportName + UNDERLINE + Tags.RESOURCE + UNDERLINE + type
								+ XML_FILE_SUFFIX;
						writer.writeEmptyElement(Tags.INCLUDE_FILE);
						writer.writeAttribute(Tags.FILE, typeXmlFile);

						File typeXmlFileF = new File(modelFile.getParentFile(), typeXmlFile);
						try (FileOutputStream typeOut = new FileOutputStream(typeXmlFileF)) {
							createdFiles.add(typeXmlFileF);
							XMLStreamWriter typeWriter = openXmlStreamWriter(typeOut);
							writeResourcesByType(typeWriter, resourceMap, type);
							typeWriter.writeEndDocument();
						}
					}
				}
			}

			if (this.doOrders) {
				OrderMap orderMap = tx().getOrderMap();
				Set<String> orderTypesToExport = orderMap.getTypes(tx());
				if (!this.orderTypes.isEmpty())
					orderTypesToExport.retainAll(this.orderTypes);

				orderTypesToExport = new TreeSet<>(orderTypesToExport);
				for (String type : orderTypesToExport) {

					if (!multiFile) {
						writeOrdersByType(writer, orderMap, type);
					} else {
						String typeXmlFile = exportName + UNDERLINE + Tags.ORDER + UNDERLINE + type + XML_FILE_SUFFIX;
						writer.writeEmptyElement(Tags.INCLUDE_FILE);
						writer.writeAttribute(Tags.FILE, typeXmlFile);

						File typeXmlFileF = new File(modelFile.getParentFile(), typeXmlFile);
						try (FileOutputStream typeOut = new FileOutputStream(typeXmlFileF)) {
							createdFiles.add(typeXmlFileF);
							XMLStreamWriter typeWriter = openXmlStreamWriter(typeOut);
							writeOrdersByType(typeWriter, orderMap, type);
							typeWriter.writeEndDocument();
						}
					}
				}
			}

			// and now end
			writer.writeEndElement();
			writer.writeEndDocument();
			writer.flush();

			writer.close();

			this.statistics.durationNanos = System.nanoTime() - start;

		} catch (Exception e) {
			for (File createdFile : createdFiles) {
				if (createdFile.exists())
					createdFile.delete();
			}
			String msg = "Failed to write model to file {0} due to {1}";
			msg = MessageFormat.format(msg, this.modelFile, e.getMessage());
			throw new StrolchException(msg, e);
		}
	}

	private void writeOrdersByType(XMLStreamWriter writer, OrderMap orderMap, String type) {
		OrderVisitor visitor = new OrderToSaxWriterVisitor(writer);
		Set<String> keysByType = new TreeSet<>(orderMap.getKeysBy(tx(), type));
		for (String id : keysByType) {
			Order order = orderMap.getBy(tx(), type, id);
			visitor.visit(order);
			this.statistics.nrOfOrders++;
		}
	}

	private void writeResourcesByType(XMLStreamWriter writer, ResourceMap resourceMap, String type) {
		ResourceVisitor visitor = new ResourceToSaxWriterVisitor(writer);
		Set<String> keysByType = new TreeSet<>(resourceMap.getKeysBy(tx(), type));
		for (String id : keysByType) {
			Resource resource = resourceMap.getBy(tx(), type, id);
			visitor.visit(resource);
			this.statistics.nrOfResources++;
		}
	}

	private XMLStreamWriter openXmlStreamWriter(FileOutputStream out) throws FactoryConfigurationError,
			XMLStreamException {
		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		XMLStreamWriter writer = factory.createXMLStreamWriter(out, StrolchConstants.DEFAULT_ENCODING);
		writer = new IndentingXMLStreamWriter(writer);

		// start document
		writer.writeStartDocument(StrolchConstants.DEFAULT_ENCODING, StrolchConstants.DEFAULT_XML_VERSION);
		writer.writeStartElement(Tags.STROLCH_MODEL);
		return writer;
	}

	/**
	 * @param modelFile
	 *            the modelFile to set
	 */
	public void setModelFile(File modelFile) {
		this.modelFile = modelFile;
	}

	/**
	 * @param multiFile
	 */
	public void setMultiFile(boolean multiFile) {
		this.multiFile = multiFile;
	}

	/**
	 * @param doOrders
	 *            the doOrders to set
	 */
	public void setDoOrders(boolean doOrders) {
		this.doOrders = doOrders;
	}

	/**
	 * @param doResources
	 *            the doResources to set
	 */
	public void setDoResources(boolean doResources) {
		this.doResources = doResources;
	}

	/**
	 * @param orderTypes
	 *            the orderTypes to set
	 */
	public void setOrderTypes(Set<String> orderTypes) {
		this.orderTypes = orderTypes;
	}

	/**
	 * @param resourceTypes
	 *            the resourceTypes to set
	 */
	public void setResourceTypes(Set<String> resourceTypes) {
		this.resourceTypes = resourceTypes;
	}

	/**
	 * @return the statistics
	 */
	public XmlModelStatistics getStatistics() {
		return this.statistics;
	}
}
