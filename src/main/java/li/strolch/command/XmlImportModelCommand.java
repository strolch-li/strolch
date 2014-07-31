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
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlImportModelCommand extends Command {

	// input
	private File modelFile;
	private boolean addOrders;
	private boolean addResources;
	private boolean updateOrders;
	private boolean updateResources;
	private Set<String> orderTypes;
	private Set<String> resourceTypes;

	// output
	private ModelStatistics statistics;

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

		elementListener.setAddOrders(addOrders);
		elementListener.setAddResources(addResources);
		elementListener.setUpdateOrders(updateOrders);
		elementListener.setUpdateResources(updateResources);
		elementListener.setOrderTypes(orderTypes);
		elementListener.setResourceTypes(resourceTypes);

		XmlModelSaxFileReader handler = new XmlModelSaxFileReader(elementListener, modelFile);
		handler.parseFile();

		statistics = handler.getStatistics();
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
	 * @param addOrders
	 *            the addOrders to set
	 */
	public void setAddOrders(boolean addOrders) {
		this.addOrders = addOrders;
	}

	/**
	 * @param addResources
	 *            the addResources to set
	 */
	public void setAddResources(boolean addResources) {
		this.addResources = addResources;
	}

	/**
	 * @param updateOrders
	 *            the updateOrders to set
	 */
	public void setUpdateOrders(boolean updateOrders) {
		this.updateOrders = updateOrders;
	}

	/**
	 * @param updateResources
	 *            the updateResources to set
	 */
	public void setUpdateResources(boolean updateResources) {
		this.updateResources = updateResources;
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
	public ModelStatistics getStatistics() {
		return this.statistics;
	}
}
