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
import li.strolch.command.visitor.UndoUpdateElementVisitor;
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
public class RemoveParameterCommand extends Command {

	private ParameterizedElement element;
	private String parameterId;

	private Parameter<?> removedParameter;
	private boolean removed;

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
	public void validate() {
		DBC.PRE.assertNotNull("Element may not be null!", this.element);
		DBC.PRE.assertNotEmpty("ParameterId must be set!", this.parameterId);

		if (!this.element.hasParameter(this.parameterId)) {
			String msg = "The Parameter {0} can not be removed as it does not exist on element {1}";
			msg = MessageFormat.format(msg, this.parameterId, this.element.getLocator());
			throw new StrolchPersistenceException(msg);
		}
	}

	@Override
	public void doCommand() {

		StrolchRootElement rootElement = this.element.getRootElement();
		tx().lock(rootElement);

		this.removedParameter = this.element.removeParameter(this.parameterId);
		new UpdateElementVisitor(tx()).update(rootElement);
		this.removed = true;
	}

	@Override
	public void undo() {
		if (this.removed && this.removedParameter != null) {
			this.element.addParameter(this.removedParameter);
			new UndoUpdateElementVisitor(tx()).undo(this.removedParameter.getRootElement());
		}
	}
}
