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
package li.strolch.service;

import static li.strolch.model.ModelGenerator.BAG_ID;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;
import org.junit.Test;

public class TxExtendedTest extends AbstractRealmServiceTest<ServiceArgument, ServiceResult> {

	private Class<? extends Service<ServiceArgument, ServiceResult>> svcClass;

	@Override
	protected Class<? extends Service<ServiceArgument, ServiceResult>> getSvcClass() {
		return this.svcClass;
	}

	@Override
	protected ServiceArgument getArgInstance() {
		return new ServiceArgument();
	}

	@Test
	public void shouldCommit() {
		this.svcClass = CommitService.class;
		runServiceInAllRealmTypes();
	}

	@Test
	public void shouldRollback() {
		this.svcClass = RollbackService.class;
		runServiceInAllRealmTypes();
	}

	public static class CommitService extends AbstractService<ServiceArgument, ServiceResult> {

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		public ServiceArgument getArgumentInstance() {
			return new ServiceArgument();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

			String type = "MyType";

			String resId = StrolchAgent.getUniqueId();
			Resource resource = ModelGenerator.createResource(resId, resId, type);
			Resource resource1 = ModelGenerator.createResource(resId + "1", resId + "1", type);
			Resource resource2 = ModelGenerator.createResource(resId + "2", resId + "2", type);
			Resource resource3 = ModelGenerator.createResource(resId + "3", resId + "3", type);
			StringListParameter resRefsP = new StringListParameter("refP", "Ref P",
					Arrays.asList(resource1.getId(), resource2.getId(), resource3.getId()));
			resRefsP.setInterpretation(StrolchConstants.INTERPRETATION_RESOURCE_REF);
			resRefsP.setUom(type);
			resource.addParameter(BAG_ID, resRefsP);

			StringParameter resRefP = new StringParameter("refP", "Ref P", resource.getId());
			resRefP.setInterpretation(StrolchConstants.INTERPRETATION_RESOURCE_REF);
			resRefP.setUom(type);
			resource1.addParameter(BAG_ID, resRefP.getClone());
			resource2.addParameter(BAG_ID, resRefP.getClone());
			resource3.addParameter(BAG_ID, resRefP.getClone());

			String orderId = StrolchAgent.getUniqueId();
			Order order = ModelGenerator.createOrder(orderId, orderId, type);
			Order order1 = ModelGenerator.createOrder(orderId + "1", orderId + "1", type);
			Order order2 = ModelGenerator.createOrder(orderId + "2", orderId + "2", type);
			Order order3 = ModelGenerator.createOrder(orderId + "3", orderId + "3", type);
			StringListParameter orderRefsP = new StringListParameter("refP", "Ref P",
					Arrays.asList(order1.getId(), order2.getId(), order3.getId()));
			orderRefsP.setInterpretation(StrolchConstants.INTERPRETATION_ORDER_REF);
			orderRefsP.setUom(type);
			order.addParameter(BAG_ID, orderRefsP);

			StringParameter orderRefP = new StringParameter("refP", "Ref P", order.getId());
			orderRefP.setInterpretation(StrolchConstants.INTERPRETATION_ORDER_REF);
			orderRefP.setUom(type);
			order1.addParameter(BAG_ID, orderRefP.getClone());
			order2.addParameter(BAG_ID, orderRefP.getClone());
			order3.addParameter(BAG_ID, orderRefP.getClone());

			String activityId = StrolchAgent.getUniqueId();
			Activity activity = ModelGenerator.createActivity(activityId, activityId, type, TimeOrdering.SERIES);
			Activity activity1 = ModelGenerator
					.createActivity(activityId + "1", activityId + "1", type, TimeOrdering.SERIES);
			Activity activity2 = ModelGenerator
					.createActivity(activityId + "2", activityId + "2", type, TimeOrdering.SERIES);
			Activity activity3 = ModelGenerator
					.createActivity(activityId + "3", activityId + "3", type, TimeOrdering.SERIES);
			StringListParameter actRefsP = new StringListParameter("refP", "Ref P",
					Arrays.asList(activity1.getId(), activity2.getId(), activity3.getId()));
			actRefsP.setInterpretation(StrolchConstants.INTERPRETATION_ACTIVITY_REF);
			actRefsP.setUom(type);
			activity.addParameter(BAG_ID, actRefsP);

			StringParameter actRefP = new StringParameter("refP", "Ref P", activity.getId());
			actRefP.setInterpretation(StrolchConstants.INTERPRETATION_ACTIVITY_REF);
			actRefP.setUom(type);
			activity1.addParameter(BAG_ID, actRefP.getClone());
			activity2.addParameter(BAG_ID, actRefP.getClone());
			activity3.addParameter(BAG_ID, actRefP.getClone());

			try (StrolchTransaction tx = openTx(arg.realm)) {

				// find param
				Resource location = tx.getResourceBy("Location", "Facility", true);
				Optional<StringParameter> productionModeP = tx
						.findParameterOnHierarchy(location, "parent", "parameters", "productionMode");
				assertTrue(productionModeP.isPresent());
				assertEquals("auto", productionModeP.get().getValue());

				location = tx.getResourceBy("Location", "BlockA", true);
				productionModeP = tx.findParameterOnHierarchy(location, "parent", "parameters", "productionMode");
				assertTrue(productionModeP.isPresent());
				assertEquals("semi-auto", productionModeP.get().getValue());

				location = tx.getResourceBy("Location", "BlockA.1", true);
				productionModeP = tx.findParameterOnHierarchy(location, "parent", "parameters", "productionMode");
				assertTrue(productionModeP.isPresent());
				assertEquals("semi-auto", productionModeP.get().getValue());

				location = tx.getResourceBy("Location", "BlockA.2", true);
				productionModeP = tx.findParameterOnHierarchy(location, "parent", "parameters", "productionMode");
				assertTrue(productionModeP.isPresent());
				assertEquals("manual", productionModeP.get().getValue());

				location = tx.getResourceBy("Location", "BlockB", true);
				productionModeP = tx.findParameterOnHierarchy(location, "parent", "parameters", "productionMode");
				assertTrue(productionModeP.isPresent());
				assertEquals("auto", productionModeP.get().getValue());

				location = tx.getResourceBy("Location", "BlockB.1", true);
				productionModeP = tx.findParameterOnHierarchy(location, "parent", "parameters", "productionMode");
				assertTrue(productionModeP.isPresent());
				assertEquals("auto", productionModeP.get().getValue());

				// resource
				assertNull(tx.getResourceBy(type, resId));
				assertFalse(tx.hasResource(type, resId));
				assertEquals(0, tx.getResourcesBy(resRefsP).size());
				tx.add(resource);
				tx.add(resource1);
				tx.add(resource2);
				tx.add(resource3);
				tx.update(resource3);
				assertNotNull(tx.getResourceBy(type, resId));
				assertTrue(tx.hasResource(type, resId));
				List<Resource> resourcesBy = tx.getResourcesBy(resRefsP);
				assertEquals(3, resourcesBy.size());
				for (Resource res : resourcesBy) {
					assertEquals(resource, tx.getResourceBy(res.getParameter(BAG_ID, "refP")));
				}

				// order
				assertNull(tx.getOrderBy(type, orderId));
				assertFalse(tx.hasOrder(type, orderId));
				assertEquals(0, tx.getOrdersBy(orderRefsP).size());
				tx.add(order);
				tx.add(order1);
				tx.add(order2);
				tx.add(order3);
				tx.update(order3);
				assertNotNull(tx.getOrderBy(type, orderId));
				assertTrue(tx.hasOrder(type, orderId));
				List<Order> ordersBy = tx.getOrdersBy(orderRefsP);
				assertEquals(3, ordersBy.size());
				for (Order o : ordersBy) {
					assertEquals(order, tx.getOrderBy(o.getParameter(BAG_ID, "refP")));
				}

				// activity
				assertNull(tx.getActivityBy(type, activityId));
				assertFalse(tx.hasActivity(type, activityId));
				assertEquals(0, tx.getActivitiesBy(actRefsP).size());
				tx.add(activity);
				tx.add(activity1);
				tx.add(activity2);
				tx.add(activity3);
				tx.update(activity3);
				assertNotNull(tx.getActivityBy(type, activityId));
				assertTrue(tx.hasActivity(type, activityId));
				List<Activity> activitiesBy = tx.getActivitiesBy(actRefsP);
				assertEquals(3, activitiesBy.size());
				for (Activity a : activitiesBy) {
					assertEquals(activity, tx.getActivityBy(a.getParameter(BAG_ID, "refP")));
				}

				tx.commitOnClose();
			}

			// now make sure the new resource exists
			try (StrolchTransaction tx = openTx(arg.realm)) {

				assertNotNull(tx.getResourceBy(type, resource.getId()));
				assertNotNull(tx.getOrderBy(type, order.getId()));
				assertNotNull(tx.getActivityBy(type, activity.getId()));
			}

			// remove elements
			try (StrolchTransaction tx = openTx(arg.realm)) {
				tx.remove(resource);
				tx.remove(order);
				tx.remove(activity);

				// can't assert that they aren't on the maps anymore, as that is not currently supported

				tx.commitOnClose();
			}

			// assert they don't exist anymore
			try (StrolchTransaction tx = openTx(arg.realm)) {

				assertNull(tx.getResourceBy(type, resource.getId()));
				assertNull(tx.getOrderBy(type, order.getId()));
				assertNull(tx.getActivityBy(type, activity.getId()));

			}

			// add with same ID
			try (StrolchTransaction tx = openTx(arg.realm)) {

				tx.add(resource.getClone());
				tx.add(order.getClone());
				tx.add(activity.getClone());

				assertTrue(tx.hasResource(type, resId));
				assertTrue(tx.hasOrder(type, orderId));
				assertTrue(tx.hasActivity(type, activityId));

				tx.commitOnClose();
			}

			// assert they exist
			try (StrolchTransaction tx = openTx(arg.realm)) {

				assertTrue(tx.hasResource(type, resId));
				assertTrue(tx.hasOrder(type, orderId));
				assertTrue(tx.hasActivity(type, activityId));

			}

			return ServiceResult.success();
		}
	}

	public static class RollbackService extends AbstractService<ServiceArgument, ServiceResult> {

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		public ServiceArgument getArgumentInstance() {
			return new ServiceArgument();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

			String type = "MyType";

			String resId = StrolchAgent.getUniqueId();
			Resource resource = ModelGenerator.createResource(resId, resId, type);
			Resource resource1 = ModelGenerator.createResource(resId + "1", resId + "1", type);
			Resource resource2 = ModelGenerator.createResource(resId + "2", resId + "2", type);
			Resource resource3 = ModelGenerator.createResource(resId + "3", resId + "3", type);
			StringListParameter resRefP = new StringListParameter("refP", "Ref P",
					Arrays.asList(resource1.getId(), resource2.getId(), resource3.getId()));
			resRefP.setInterpretation(StrolchConstants.INTERPRETATION_RESOURCE_REF);
			resRefP.setUom(type);
			resource.addParameter(BAG_ID, resRefP);

			String orderId = StrolchAgent.getUniqueId();
			Order order = ModelGenerator.createOrder(orderId, orderId, type);
			Order order1 = ModelGenerator.createOrder(orderId + "1", orderId + "1", type);
			Order order2 = ModelGenerator.createOrder(orderId + "2", orderId + "2", type);
			Order order3 = ModelGenerator.createOrder(orderId + "3", orderId + "3", type);
			StringListParameter orderRefP = new StringListParameter("refP", "Ref P",
					Arrays.asList(order1.getId(), order2.getId(), order3.getId()));
			orderRefP.setInterpretation(StrolchConstants.INTERPRETATION_ORDER_REF);
			orderRefP.setUom(type);
			order.addParameter(BAG_ID, orderRefP);

			String activityId = StrolchAgent.getUniqueId();
			Activity activity = ModelGenerator.createActivity(activityId, activityId, type, TimeOrdering.SERIES);
			Activity activity1 = ModelGenerator
					.createActivity(activityId + "1", activityId + "1", type, TimeOrdering.SERIES);
			Activity activity2 = ModelGenerator
					.createActivity(activityId + "2", activityId + "2", type, TimeOrdering.SERIES);
			Activity activity3 = ModelGenerator
					.createActivity(activityId + "3", activityId + "3", type, TimeOrdering.SERIES);
			StringListParameter actRefP = new StringListParameter("refP", "Ref P",
					Arrays.asList(activity1.getId(), activity2.getId(), activity3.getId()));
			actRefP.setInterpretation(StrolchConstants.INTERPRETATION_ACTIVITY_REF);
			actRefP.setUom(type);
			activity.addParameter(BAG_ID, actRefP);

			try (StrolchTransaction tx = openTx(arg.realm)) {

				// resource
				assertNull(tx.getResourceBy(type, resId));
				assertFalse(tx.hasResource(type, resId));
				assertEquals(0, tx.getResourcesBy(resRefP).size());
				tx.add(resource);
				tx.add(resource1);
				tx.add(resource2);
				tx.add(resource3);
				assertNotNull(tx.getResourceBy(type, resId));
				assertTrue(tx.hasResource(type, resId));
				assertEquals(3, tx.getResourcesBy(resRefP).size());

				// order
				assertNull(tx.getOrderBy(type, orderId));
				assertFalse(tx.hasOrder(type, orderId));
				assertEquals(0, tx.getOrdersBy(orderRefP).size());
				tx.add(order);
				tx.add(order1);
				tx.add(order2);
				tx.add(order3);
				assertNotNull(tx.getOrderBy(type, orderId));
				assertTrue(tx.hasOrder(type, orderId));
				assertEquals(3, tx.getOrdersBy(orderRefP).size());

				// activity
				assertNull(tx.getActivityBy(type, activityId));
				assertFalse(tx.hasActivity(type, activityId));
				assertEquals(0, tx.getActivitiesBy(actRefP).size());
				tx.add(activity);
				tx.add(activity1);
				tx.add(activity2);
				tx.add(activity3);
				assertNotNull(tx.getActivityBy(type, activityId));
				assertTrue(tx.hasActivity(type, activityId));
				assertEquals(3, tx.getActivitiesBy(actRefP).size());

				tx.rollbackOnClose();
			}

			// now make sure the new resource does not exist
			try (StrolchTransaction tx = openTx(arg.realm)) {

				assertNull(tx.getResourceBy(type, resource.getId()));
				assertNull(tx.getResourceBy(type, resource1.getId()));
				assertNull(tx.getResourceBy(type, resource2.getId()));
				assertNull(tx.getResourceBy(type, resource3.getId()));
				assertNull(tx.getOrderBy(type, order.getId()));
				assertNull(tx.getOrderBy(type, order1.getId()));
				assertNull(tx.getOrderBy(type, order2.getId()));
				assertNull(tx.getOrderBy(type, order3.getId()));
				assertNull(tx.getActivityBy(type, activity.getId()));
				assertNull(tx.getActivityBy(type, activity1.getId()));
				assertNull(tx.getActivityBy(type, activity2.getId()));
				assertNull(tx.getActivityBy(type, activity3.getId()));
			}

			return ServiceResult.success();
		}
	}
}
