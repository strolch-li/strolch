/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.model;

import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static li.strolch.model.ModelHelper.setInterpretationAndUom;
import static li.strolch.model.builder.BuilderHelper.buildParamName;

public abstract class AbstractStrolchRootElement extends GroupedParameterizedElement implements StrolchRootElement {

	public AbstractStrolchRootElement() {
		super();
	}

	public AbstractStrolchRootElement(String id, String name, String type) {
		super(id, name, type);
	}

	@Override
	public void setRelation(String param, StrolchRootElement element) {
		StringParameter relationP = relationsBag().getParameter(param);
		if (relationP == null) {
			String name = buildParamName(param);
			relationP = new StringParameter(param, name, "");
			setInterpretationAndUom(relationP, element.getObjectType(), element.getType());
			relationsBag().addParameter(relationP);
		}

		relationP.setValue(element.getId());
	}

	@Override
	public void addRelation(String param, StrolchRootElement element) {
		StringListParameter relationsP = relationsBag().getParameter(param);
		if (relationsP == null) {
			String name = buildParamName(param);
			relationsP = new StringListParameter(param, name, Collections.emptyList());
			setInterpretationAndUom(relationsP, element.getObjectType(), element.getType());
			relationsBag().addParameter(relationsP);
		}

		relationsP.addValueIfNotContains(element.getId());
	}

	@Override
	public void setRelations(String param, Collection<? extends StrolchRootElement> elements) {
		StringListParameter relationsP = relationsBag().getParameter(param);
		if (elements.isEmpty()) {
			if (relationsP != null)
				relationsP.clear();
			return;
		}

		// validate we have same objects
		List<String> objectTypes = elements.stream().map(StrolchRootElement::getObjectType).distinct().toList();
		List<String> types = elements.stream().map(StrolchRootElement::getType).distinct().toList();
		if (objectTypes.size() != 1)
			throw new IllegalStateException("Only allow to have one type of object: " + elements
					.stream()
					.map(StrolchElement::getId)
					.collect(Collectors.joining(", ")));
		if (types.size() != 1)
			throw new IllegalStateException("Only allow to have one type of object: " + elements
					.stream()
					.map(StrolchElement::getId)
					.collect(Collectors.joining(", ")));

		if (relationsP == null) {
			String name = buildParamName(param);
			relationsP = new StringListParameter(param, name, emptyList());
			setInterpretationAndUom(relationsP, objectTypes.getFirst(), types.getFirst());

			relationsBag().addParameter(relationsP);
		}

		for (StrolchRootElement element : elements) {
			relationsP.addValueIfNotContains(element.getId());
		}
	}
}
