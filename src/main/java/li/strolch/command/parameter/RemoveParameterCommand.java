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
package li.strolch.command.parameter;

import java.text.MessageFormat;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.ParameterizedElement;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveParameterCommand extends Command {

	private ParameterizedElement element;
	private String parameterId;

	/**
	 * @param container
	 * @param tx
	 */
	public RemoveParameterCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * @param element
	 *            the element to set
	 */
	public void setElement(ParameterizedElement element) {
		this.element = element;
	}

	/**
	 * @param parameterId
	 *            the parameterId to set
	 */
	public void setParameterId(String parameterId) {
		this.parameterId = parameterId;
	}

	@Override
	public void doCommand() {
		DBC.PRE.assertNotNull("Element may not be null!", element);
		DBC.PRE.assertNotEmpty("ParameterId must be set!", parameterId);

		if (!element.hasParameter(this.parameterId)) {
			String msg = "The Parameter {0} can not be removed as it does not exist on element {1}";
			msg = MessageFormat.format(msg, parameterId, element.getLocator());
			throw new StrolchPersistenceException(msg);
		}

		this.element.removeParameter(parameterId);
	}
}
