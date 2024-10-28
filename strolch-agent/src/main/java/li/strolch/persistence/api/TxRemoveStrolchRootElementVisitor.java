/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.persistence.api;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;

public class TxRemoveStrolchRootElementVisitor implements StrolchRootElementVisitor<Void> {

	private final StrolchTransaction tx;

	public TxRemoveStrolchRootElementVisitor(StrolchTransaction tx) {
		this.tx = tx;
	}

	@Override
	public Void visitOrder(Order order) {
		tx.remove(order);
		return null;
	}

	@Override
	public Void visitResource(Resource resource) {
		tx.remove(resource);
		return null;
	}

	@Override
	public Void visitActivity(Activity activity) {
		tx.remove(activity);
		return null;
	}
}
