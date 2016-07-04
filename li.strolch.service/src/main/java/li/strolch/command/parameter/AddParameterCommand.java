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
import li.strolch.command.visitor.UpdateElementVisitor;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddParameterCommand extends Command {

	private ParameterizedElement element;
	private Parameter<?> parameter;
	private StrolchRootElement replacedElement;

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
	public void validate() {
		DBC.PRE.assertNotNull("Element may not be null!", this.element);
		DBC.PRE.assertNotNull("Parameter may not be null!", this.parameter);

		if (this.element.hasParameter(this.parameter.getId())) {
			String msg = "The Parameter {0} already exists on element {1}";
			msg = MessageFormat.format(msg, this.parameter.getId(), this.element.getLocator());
			throw new StrolchPersistenceException(msg);
		}
	}

	@Override
	public void doCommand() {

		StrolchRootElement rootElement = this.element.getRootElement();
		tx().lock(rootElement);

		this.element.addParameter(this.parameter);
		this.replacedElement = new UpdateElementVisitor(tx()).update(rootElement);
	}

	@Override
	public void undo() {
		if (this.parameter != null) {
			if (this.element.hasParameter(this.parameter.getId())) {
				this.element.removeParameter(this.parameter.getId());
			}
		}

		if (this.replacedElement != null && this.element != this.replacedElement) {
			new UpdateElementVisitor(tx()).update(this.replacedElement);
		}
	}
}
