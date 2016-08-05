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
package li.strolch.command.visitor;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateElementVisitor implements StrolchRootElementVisitor<Void> {

	private StrolchTransaction tx;

	public UpdateElementVisitor(StrolchTransaction tx) {
		this.tx = tx;
	}

	public Void update(StrolchRootElement rootElement) {
		return rootElement.accept(this);
	}

	@Override
	public Void visitOrder(Order order) {
		this.tx.getOrderMap().update(this.tx, order);
		return null;
	}

	@Override
	public Void visitResource(Resource resource) {
		this.tx.getResourceMap().update(this.tx, resource);
		return null;
	}

	@Override
	public Void visitActivity(Activity activity) {
		this.tx.getActivityMap().update(this.tx, activity);
		return null;
	}
}
