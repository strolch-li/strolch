/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.parameter.Parameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
class ElementMapHelpers {

	public static void assertIsRefParam(String expectedInterpretation, Parameter<?> refP) {

		String interpretation = refP.getInterpretation();
		if (!interpretation.equals(expectedInterpretation)) {
			String msg = "{0} is not an expected element reference as its interpretation is {1} instead of {2}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getLocator(), expectedInterpretation,
					interpretation));
		}

		if (refP.getUom().equals(UOM_NONE)) {
			String msg = "{0} is not an expected element reference as its UOM is not set to a type it is set to {1}!"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, refP.getLocator(), UOM_NONE));
		}
	}
}
