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
package li.strolch.service;

import java.util.ArrayList;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.service.AddOrderCollectionService.AddOrderCollectionArg;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddOrderCollectionServiceTest extends AbstractRealmServiceTest {

	@Test
	public void runTest() {

		AddOrderCollectionArg arg = new AddOrderCollectionArg();

		ArrayList<Order> orders = new ArrayList<>();
		orders.add(ModelGenerator.createOrder("firstOrder", "First Order", "AdditionalOrders"));
		orders.add(ModelGenerator.createOrder("secondOrder", "Second Order", "AdditionalOrders"));
		orders.add(ModelGenerator.createOrder("thirdOrder", "Third Order", "AdditionalOrders"));

		arg.orders = orders;

		runServiceInAllRealmTypes(AddOrderCollectionService.class, arg);
	}
}
