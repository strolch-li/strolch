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

import li.strolch.model.ModelGenerator;
import li.strolch.service.UpdateOrderService.UpdateOrderArg;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateOrderServiceTest extends AbstractRealmServiceTest {

	private UpdateOrderService svc;
	private UpdateOrderArg arg;

	@Before
	public void before() {
		svc = new UpdateOrderService();
		arg = new UpdateOrderArg();
		arg.order = ModelGenerator.createOrder("myCarOrder", "Modified Car Order", "ProductionOrder");
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends ServiceArgument> T getArg() {
		return (T) this.arg;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends ServiceArgument, U extends ServiceResult> Service<T, U> getSvc() {
		return (Service<T, U>) this.svc;
	}
}
