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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.visitor.SetParameterValueVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SetParameterCommand extends Command {

	private Parameter<?> parameter;

	private String name;
	private String interpretation;
	private String uom;
	private Boolean hidden;
	private Integer index;

	private String valueAsString;

	private String oldName;
	private String oldInterpretation;
	private String oldUom;
	private Boolean oldHidden;
	private Integer oldIndex;
	private String oldValueAsString;

	/**
	 * @param container
	 * @param tx
	 */
	public SetParameterCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * @param parameter
	 *            the parameter to set
	 */
	public void setParameter(Parameter<?> parameter) {
		this.parameter = parameter;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @param interpretation
	 *            the interpretation to set
	 */
	public void setInterpretation(String interpretation) {
		this.interpretation = interpretation;
	}

	/**
	 * @param uom
	 *            the uom to set
	 */
	public void setUom(String uom) {
		this.uom = uom;
	}

	/**
	 * @param hidden
	 *            the hidden to set
	 */
	public void setHidden(Boolean hidden) {
		this.hidden = hidden;
	}

	/**
	 * @param index
	 *            the index to set
	 */
	public void setIndex(Integer index) {
		this.index = index;
	}

	/**
	 * @param valueAsString
	 *            the valueAsString to set
	 */
	public void setValueAsString(String valueAsString) {
		this.valueAsString = valueAsString;
	}

	@Override
	public void doCommand() {
		DBC.PRE.assertNotNull("Parameter may not be null!", this.parameter);

		if (this.name != null) {
			this.oldName = this.parameter.getName();
			this.parameter.setName(this.name);
		}
		if (this.interpretation != null) {
			this.oldInterpretation = this.parameter.getInterpretation();
			this.parameter.setInterpretation(this.interpretation);
		}
		if (this.uom != null) {
			this.oldUom = this.parameter.getUom();
			this.parameter.setUom(this.uom);
		}
		if (this.hidden != null) {
			this.oldHidden = this.parameter.isHidden();
			this.parameter.setHidden(this.hidden);
		}
		if (this.index != null) {
			this.oldIndex = this.parameter.getIndex();
			this.parameter.setIndex(this.index.intValue());
		}

		if (this.valueAsString != null) {
			this.oldValueAsString = this.parameter.getValueAsString();
			SetParameterValueVisitor visitor = new SetParameterValueVisitor();
			visitor.setValue(this.parameter, this.valueAsString);
		}
	}

	@Override
	public void undo() {
		if (this.parameter != null) {
			if (this.oldName != null) {
				this.parameter.setName(this.oldName);
			}
			if (this.oldInterpretation != null) {
				this.parameter.setInterpretation(this.oldInterpretation);
			}
			if (this.oldUom != null) {
				this.parameter.setUom(this.oldUom);
			}
			if (this.oldHidden != null) {
				this.parameter.setHidden(this.oldHidden);
			}
			if (this.oldIndex != null) {
				this.parameter.setIndex(this.oldIndex.intValue());
			}

			if (this.oldValueAsString != null) {
				SetParameterValueVisitor visitor = new SetParameterValueVisitor();
				visitor.setValue(this.parameter, this.oldValueAsString);
			}
		}
	}
}
