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
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchElementVisitor<T> extends StrolchVisitor {

	public T visitOrder(Order order);

	public T visitResource(Resource resource);

	public T visitActivity(Activity activity);

	public T visitAction(Action action);

	public default ResourceVisitor<T> asResourceVisitor() {
		return resource -> this.visitResource(resource);
	}

	public default OrderVisitor<T> asOrderVisitor() {
		return order -> this.visitOrder(order);
	}

	public default ActivityVisitor<T> asActivityVisitor() {
		return activity -> this.visitActivity(activity);
	}
}
