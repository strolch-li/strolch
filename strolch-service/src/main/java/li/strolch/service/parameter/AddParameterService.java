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

import li.strolch.model.Locator;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.TxUpdateStrolchRootElementVisitor;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.service.parameter.AddParameterService.AddParameterArg;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddParameterService extends AbstractService<AddParameterArg, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public AddParameterArg getArgumentInstance() {
		return new AddParameterArg();
	}

	@Override
	protected ServiceResult internalDoService(AddParameterArg arg) {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			ParameterizedElement element = tx.findElement(arg.locator);
			element.addParameter(arg.parameter);

			StrolchRootElement rootElement = element.getRootElement();
			rootElement.accept(new TxUpdateStrolchRootElementVisitor(tx));

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	public static class AddParameterArg extends ServiceArgument {

		public Locator locator;
		public Parameter<?> parameter;
	}
}
