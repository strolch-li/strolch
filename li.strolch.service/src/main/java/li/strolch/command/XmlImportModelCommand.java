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
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.impl.InMemoryElementListener;
import li.strolch.model.ModelStatistics;
import li.strolch.model.xml.XmlModelSaxFileReader;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlImportModelCommand extends Command {

	// input
	private File modelFile;
	private boolean addOrders;
	private boolean addResources;
	private boolean addActivities;
	private boolean updateOrders;
	private boolean updateResources;
	private boolean updateActivities;
	private Set<String> orderTypes;
	private Set<String> resourceTypes;
	private Set<String> activityTypes;

	// output
	private ModelStatistics statistics;
	private boolean allowInclude;

	/**
	 * @param container
	 * @param tx
	 */
	public XmlImportModelCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void validate() {
		DBC.PRE.assertExists("Model must exist!", this.modelFile);
	}

	@Override
	public void doCommand() {

		InMemoryElementListener elementListener = new InMemoryElementListener(tx());

		elementListener.setAddOrders(this.addOrders);
		elementListener.setAddResources(this.addResources);
		elementListener.setAddActivities(this.addActivities);
		elementListener.setUpdateOrders(this.updateOrders);
		elementListener.setUpdateResources(this.updateResources);
		elementListener.setUpdateActivities(this.updateActivities);
		elementListener.setOrderTypes(this.orderTypes);
		elementListener.setResourceTypes(this.resourceTypes);
		elementListener.setActivityTypes(this.activityTypes);

		XmlModelSaxFileReader handler = new XmlModelSaxFileReader(elementListener, this.modelFile, this.allowInclude);
		handler.parseFile();

		this.statistics = handler.getStatistics();
	}

	@Override
	public void undo() {
		logger.warn("Not undoing import of file " + this.modelFile);
	}

	/**
	 * @param modelFileName
	 */
	public void setModelFile(File modelFileName) {
		this.modelFile = modelFileName;
	}

	/**
	 * @param allowInclude
	 */
	public void setAllowInclude(boolean allowInclude) {
		this.allowInclude = allowInclude;
	}

	/**
	 * @param addOrders
	 * 		the addOrders to set
	 */
	public void setAddOrders(boolean addOrders) {
		this.addOrders = addOrders;
	}

	/**
	 * @param addResources
	 * 		the addResources to set
	 */
	public void setAddResources(boolean addResources) {
		this.addResources = addResources;
	}

	/**
	 * @param addActivities
	 * 		the addActivities to set
	 */
	public void setAddActivities(boolean addActivities) {
		this.addActivities = addActivities;
	}

	/**
	 * @param updateOrders
	 * 		the updateOrders to set
	 */
	public void setUpdateOrders(boolean updateOrders) {
		this.updateOrders = updateOrders;
	}

	/**
	 * @param updateResources
	 * 		the updateResources to set
	 */
	public void setUpdateResources(boolean updateResources) {
		this.updateResources = updateResources;
	}

	/**
	 * @param updateActivities
	 * 		the updateActivities to set
	 */
	public void setUpdateActivities(boolean updateActivities) {
		this.updateActivities = updateActivities;
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
}
