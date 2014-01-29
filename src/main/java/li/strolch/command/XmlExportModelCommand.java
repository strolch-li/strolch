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

import java.io.File;
import java.io.FileOutputStream;
import java.text.MessageFormat;
import java.util.Date;
import java.util.Set;
import java.util.TreeSet;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.stream.XMLOutputFactory;
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

	// input
	private File modelFile;
	private boolean doOrders;
	private boolean doResources;
	private Set<String> orderTypes;
	private Set<String> resourceTypes;

	// output
	private XmlModelStatistics statistics;

	/**
	 * @param container
	 * @param tx
	 */
	public XmlExportModelCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void doCommand() {
		DBC.PRE.assertNotExists("Model may not already exist!", this.modelFile);

		this.statistics = new XmlModelStatistics();
		this.statistics.startTime = new Date();
		long start = System.nanoTime();

		try (FileOutputStream out = new FileOutputStream(this.modelFile)) {

			XMLOutputFactory factory = XMLOutputFactory.newInstance();
			XMLStreamWriter writer = factory.createXMLStreamWriter(out, StrolchConstants.DEFAULT_ENCODING);
			writer = new IndentingXMLStreamWriter(writer);

			// start document
			writer.writeStartDocument(StrolchConstants.DEFAULT_ENCODING, StrolchConstants.DEFAULT_XML_VERSION);
			writer.writeStartElement(Tags.STROLCH_MODEL);

			if (doResources) {
				ResourceMap resourceMap = tx().getResourceMap();
				Set<String> resourceTypesToExport = resourceMap.getTypes(tx());
				if (!this.resourceTypes.isEmpty())
					resourceTypesToExport.retainAll(resourceTypes);

				resourceTypesToExport = new TreeSet<>(resourceTypesToExport);
				ResourceVisitor visitor = new ResourceToSaxWriterVisitor(writer);
				for (String type : resourceTypesToExport) {
					Set<String> keysByType = new TreeSet<>(resourceMap.getKeysBy(tx(), type));
					for (String id : keysByType) {
						Resource resource = resourceMap.getBy(tx(), type, id);
						visitor.visit(resource);
						statistics.nrOfResources++;
					}
				}
			}

			if (doOrders) {
				OrderMap orderMap = tx().getOrderMap();
				Set<String> orderTypesToExport = orderMap.getTypes(tx());
				if (!this.orderTypes.isEmpty())
					orderTypesToExport.retainAll(orderTypes);

				orderTypesToExport = new TreeSet<>(orderTypesToExport);
				OrderVisitor visitor = new OrderToSaxWriterVisitor(writer);
				for (String type : orderTypesToExport) {
					Set<String> keysByType = new TreeSet<>(orderMap.getKeysBy(tx(), type));
					for (String id : keysByType) {
						Order order = orderMap.getBy(tx(), type, id);
						visitor.visit(order);
						statistics.nrOfOrders++;
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
			if (modelFile.exists())
				modelFile.delete();
			String msg = "Failed to write model to file {0} due to {1}";
			msg = MessageFormat.format(msg, modelFile, e.getMessage());
			throw new StrolchException(msg, e);
		}
	}

	/**
	 * @param modelFile
	 *            the modelFile to set
	 */
	public void setModelFile(File modelFile) {
		this.modelFile = modelFile;
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
