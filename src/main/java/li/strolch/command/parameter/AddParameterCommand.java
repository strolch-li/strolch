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

import ch.eitchnet.utils.dbc.DBC;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddParameterCommand extends Command {

	private ParameterizedElement element;
	private Parameter<?> parameter;

	/**
	 * @param container
	 * @param tx
	 */
	public AddParameterCommand(ComponentContainer container, StrolchTransaction tx) {
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
	 * @param parameter
	 *            the parameter to set
	 */
	public void setParameter(Parameter<?> parameter) {
		this.parameter = parameter;
	}

	@Override
	public void doCommand() {
		DBC.PRE.assertNotNull("Element may not be null!", element);
		DBC.PRE.assertNotNull("Parameter may not be null!", parameter);

		if (element.hasParameter(this.parameter.getId())) {
			String msg = "The Parameter {0} already exists on element {1}";
			msg = MessageFormat.format(msg, parameter.getId(), element.getLocator());
			throw new StrolchPersistenceException(msg);
		}

		this.element.addParameter(parameter);
	}

}
