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
package li.strolch.service.parameter;

import li.strolch.command.parameter.SetParameterCommand;
import li.strolch.model.Locator;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SetParameterService extends AbstractService<SetParameterService.SetParameterArg, ServiceResult> {

	public static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	protected ServiceResult internalDoService(SetParameterArg arg) {

		try (StrolchTransaction tx = openTx(arg.realm)) {

			Parameter<?> parameter = tx.findElement(arg.locator);

			SetParameterCommand command = new SetParameterCommand(getContainer(), tx);
			command.setParameter(parameter);

			command.setName(arg.name);
			command.setInterpretation(arg.interpretation);
			command.setUom(arg.uom);
			command.setHidden(arg.hidden);
			command.setIndex(arg.index);

			command.setValueAsString(arg.valueAsString);

			tx.addCommand(command);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	public static class SetParameterArg extends ServiceArgument {
		public static final long serialVersionUID = 1L;
		public Locator locator;

		public String name;
		public String interpretation;
		public String uom;
		public Boolean hidden;
		public Integer index;

		public String valueAsString;
	}
}
