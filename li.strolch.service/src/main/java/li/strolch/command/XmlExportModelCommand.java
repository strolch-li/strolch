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

import static li.strolch.utils.helper.StringHelper.UNDERLINE;

import javax.xml.stream.XMLStreamWriter;
import java.io.File;
import java.io.FilenameFilter;
import java.io.OutputStream;
import java.nio.file.Files;
import java.text.MessageFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.ModelStatistics;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.xml.StrolchElementToSaxWriterVisitor;
import li.strolch.model.xml.StrolchXmlHelper;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlExportModelCommand extends Command {

	public static final String XML_FILE_SUFFIX = ".xml";
	private static final long LOG_INTERVAL = TimeUnit.SECONDS.toMillis(10);

	// input
	private File modelFile;
	private boolean multiFile;
	private boolean overwrite;
	private boolean doOrders;
	private boolean doResources;
	private boolean doActivities;
	private Set<String> orderTypes;
	private Set<String> resourceTypes;
	private Set<String> activityTypes;

	// output
	private ModelStatistics statistics;

	private int elementsToWrite;
	private int nrOfResourcesToExport;
	private int nrOfOrdersToExport;
	private int nrOfActivitiesToExport;

	private long nextLogTime;

	public XmlExportModelCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void validate() {
		if (!this.overwrite)
			DBC.PRE.assertNotExists("Model may not already exist!", this.modelFile);
		DBC.PRE.assertTrue("Model file must end with .xml", this.modelFile.getName().endsWith(XML_FILE_SUFFIX));
	}

	private void cleanUpExisting(final String exportName) {
		File parentFile = this.modelFile.getParentFile();
		File[] existingFiles = parentFile.listFiles(new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return name.startsWith(exportName) && name.endsWith(".xml");
			}
		});

		for (File file : existingFiles) {
			file.delete();
		}
	}

	@Override
	public void doCommand() {
		this.nextLogTime = System.currentTimeMillis() + LOG_INTERVAL;
		String fileName = this.modelFile.getName();

		long start = System.nanoTime();
		this.statistics = new ModelStatistics();
		this.statistics.startTime = new Date();

		String exportName = fileName.substring(0, fileName.indexOf(XML_FILE_SUFFIX));

		// only delete existing files if doing multi-file
		if (this.multiFile)
			cleanUpExisting(exportName);

		Set<File> createdFiles = new HashSet<>();

		if (this.doResources) {
			ResourceMap resourceMap = tx().getResourceMap();
			Set<String> resourceTypesToExport = resourceMap.getTypes(tx());
			if (!this.resourceTypes.isEmpty())
				resourceTypesToExport.retainAll(this.resourceTypes);
			for (String type : resourceTypesToExport) {
				this.nrOfResourcesToExport += resourceMap.querySize(tx(), type);
			}
		}

		if (this.doOrders) {
			OrderMap orderMap = tx().getOrderMap();
			Set<String> orderTypesToExport = orderMap.getTypes(tx());
			if (!this.orderTypes.isEmpty())
				orderTypesToExport.retainAll(this.orderTypes);

			for (String type : orderTypesToExport) {
				this.nrOfOrdersToExport += orderMap.querySize(tx(), type);
			}
		}

		if (this.doActivities) {
			ActivityMap activityMap = tx().getActivityMap();
			Set<String> activityTypesToExport = activityMap.getTypes(tx());
			if (!this.activityTypes.isEmpty())
				activityTypesToExport.retainAll(this.activityTypes);

			for (String type : activityTypesToExport) {
				this.nrOfActivitiesToExport += activityMap.querySize(tx(), type);
			}
		}

		this.elementsToWrite = this.nrOfResourcesToExport + this.nrOfOrdersToExport + this.nrOfActivitiesToExport;
		logger.info("Exporting " + this.elementsToWrite + " Elements...");
		logger.info("Exporting " + this.nrOfResourcesToExport + " Resources...");
		logger.info("Exporting " + this.nrOfOrdersToExport + " Orders...");
		logger.info("Exporting " + this.nrOfActivitiesToExport + " Activities...");

		try (OutputStream out = Files.newOutputStream(this.modelFile.toPath())) {
			createdFiles.add(this.modelFile);

			XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(out);

			if (this.doResources) {
				ResourceMap resourceMap = tx().getResourceMap();
				Set<String> resourceTypesToExport = resourceMap.getTypes(tx());
				if (!this.resourceTypes.isEmpty())
					resourceTypesToExport.retainAll(this.resourceTypes);

				for (String type : resourceTypesToExport) {

					if (!this.multiFile) {
						writeResourcesByType(writer, resourceMap, type);
					} else {
						String typeXmlFile =
								exportName + UNDERLINE + Tags.RESOURCE + UNDERLINE + type + XML_FILE_SUFFIX;
						writer.writeEmptyElement(Tags.INCLUDE_FILE);
						writer.writeAttribute(Tags.FILE, typeXmlFile);

						File typeXmlFileF = new File(this.modelFile.getParentFile(), typeXmlFile);
						DBC.INTERIM.assertNotExists("The type file should not exist with name.", typeXmlFileF);
						logger.info("Writing " + resourceMap.querySize(tx(), type) + " " + type + " Resources to path: "
								+ typeXmlFileF.getAbsolutePath() + "...");
						try (OutputStream typeOut = Files.newOutputStream(typeXmlFileF.toPath())) {
							createdFiles.add(typeXmlFileF);
							XMLStreamWriter typeWriter = StrolchXmlHelper.prepareXmlStreamWriter(typeOut);
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

				for (String type : orderTypesToExport) {

					if (!this.multiFile) {
						writeOrdersByType(writer, orderMap, type);
					} else {
						String typeXmlFile = exportName + UNDERLINE + Tags.ORDER + UNDERLINE + type + XML_FILE_SUFFIX;
						writer.writeEmptyElement(Tags.INCLUDE_FILE);
						writer.writeAttribute(Tags.FILE, typeXmlFile);

						File typeXmlFileF = new File(this.modelFile.getParentFile(), typeXmlFile);
						DBC.INTERIM.assertNotExists("The type file should not exist with name.", typeXmlFileF);
						logger.info("Writing " + orderMap.querySize(tx(), type) + " " + type + " Orders to path: "
								+ typeXmlFileF.getAbsolutePath() + "...");
						try (OutputStream typeOut = Files.newOutputStream(typeXmlFileF.toPath())) {
							createdFiles.add(typeXmlFileF);
							XMLStreamWriter typeWriter = StrolchXmlHelper.prepareXmlStreamWriter(typeOut);
							writeOrdersByType(typeWriter, orderMap, type);
							typeWriter.writeEndDocument();
						}
					}
				}
			}

			if (this.doActivities) {
				ActivityMap activityMap = tx().getActivityMap();
				Set<String> activityTypesToExport = activityMap.getTypes(tx());
				if (!this.activityTypes.isEmpty())
					activityTypesToExport.retainAll(this.activityTypes);

				for (String type : activityTypesToExport) {

					if (!this.multiFile) {
						writeActivitiesByType(writer, activityMap, type);
					} else {
						String typeXmlFile =
								exportName + UNDERLINE + Tags.ACTIVITY + UNDERLINE + type + XML_FILE_SUFFIX;
						writer.writeEmptyElement(Tags.INCLUDE_FILE);
						writer.writeAttribute(Tags.FILE, typeXmlFile);

						File typeXmlFileF = new File(this.modelFile.getParentFile(), typeXmlFile);
						DBC.INTERIM.assertNotExists("The type file should not exist with name.", typeXmlFileF);
						logger.info(
								"Writing " + activityMap.querySize(tx(), type) + " " + type + " Activities to path: "
										+ typeXmlFileF.getAbsolutePath() + "...");
						try (OutputStream typeOut = Files.newOutputStream(typeXmlFileF.toPath())) {
							createdFiles.add(typeXmlFileF);
							XMLStreamWriter typeWriter = StrolchXmlHelper.prepareXmlStreamWriter(typeOut);
							writeActivitiesByType(typeWriter, activityMap, type);
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

	@Override
	public void undo() {
		logger.warn("Not undoing export to file " + this.modelFile);
	}

	private void writeOrdersByType(XMLStreamWriter writer, OrderMap orderMap, String type) {
		StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);
		List<Order> orders = orderMap.getElementsBy(tx(), type);
		for (Order order : orders) {
			order.accept(visitor);
			this.statistics.nrOfOrders++;
			logElementsWritten();
		}
	}

	private void writeResourcesByType(XMLStreamWriter writer, ResourceMap resourceMap, String type) {
		StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);
		List<Resource> resources = resourceMap.getElementsBy(tx(), type);
		for (Resource resource : resources) {
			resource.accept(visitor);
			this.statistics.nrOfResources++;
			logElementsWritten();
		}
	}

	private void writeActivitiesByType(XMLStreamWriter writer, ActivityMap activityMap, String type) {
		StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);
		List<Activity> activities = activityMap.getElementsBy(tx(), type);
		for (Activity activity : activities) {
			activity.accept(visitor);
			this.statistics.nrOfActivities++;
			logElementsWritten();
		}
	}

	private void logElementsWritten() {
		if (this.nextLogTime < System.currentTimeMillis()) {
			logger.info("Wrote " + this.statistics.getNrOfElements() + " of " + this.elementsToWrite + " Elements.");
			this.nextLogTime = System.currentTimeMillis() + LOG_INTERVAL;
		}
	}

	/**
	 * @param modelFile
	 * 		the modelFile to set
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
	 * 		the doOrders to set
	 */
	public void setDoOrders(boolean doOrders) {
		this.doOrders = doOrders;
	}

	/**
	 * @param doResources
	 * 		the doResources to set
	 */
	public void setDoResources(boolean doResources) {
		this.doResources = doResources;
	}

	/**
	 * @param doActivities
	 * 		the doActivities to set
	 */
	public void setDoActivities(boolean doActivities) {
		this.doActivities = doActivities;
	}

	/**
	 * @param orderTypes
	 * 		the orderTypes to set
	 */
	public void setOrderTypes(Set<String> orderTypes) {
		this.orderTypes = orderTypes;
	}

	/**
	 * @param resourceTypes
	 * 		the resourceTypes to set
	 */
	public void setResourceTypes(Set<String> resourceTypes) {
		this.resourceTypes = resourceTypes;
	}

	/**
	 * @param activityTypes
	 * 		the activityTypes to set
	 */
	public void setActivityTypes(Set<String> activityTypes) {
		this.activityTypes = activityTypes;
	}

	/**
	 * @return the statistics
	 */
	public ModelStatistics getStatistics() {
		return this.statistics;
	}

	/**
	 * @param overwrite
	 */
	public void setOverwrite(boolean overwrite) {
		this.overwrite = overwrite;
	}
}
