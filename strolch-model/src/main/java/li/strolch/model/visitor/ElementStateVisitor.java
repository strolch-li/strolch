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
package li.strolch.model.visitor;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ElementStateVisitor implements StrolchRootElementVisitor<State> {

	@Override
	public State visitOrder(Order order) {
		return order.getState();
	}

	@Override
	public State visitResource(Resource resource) {
		throw new IllegalArgumentException("Resources have no state!");
	}

	@Override
	public State visitActivity(Activity activity) {
		return activity.getState();
	}
}
