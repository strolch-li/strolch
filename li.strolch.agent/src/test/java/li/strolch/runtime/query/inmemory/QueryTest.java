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
package li.strolch.runtime.query.inmemory;

import static li.strolch.model.ModelGenerator.*;
import static li.strolch.model.query.ParameterSelection.integerSelection;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import li.strolch.RuntimeMock;
import li.strolch.agent.ComponentContainerTest;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.query.*;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.utils.StringMatchMode;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class QueryTest {

	public static final String PATH_EMPTY_RUNTIME = "target/QueryTest/"; //$NON-NLS-1$

	private Certificate login(ComponentContainer container) {
		return container.getPrivilegeHandler().authenticate("test", "test".toCharArray());
	}

	@Test
	public void shouldQueryResourceWithParamValue() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Resource res1 = createResource("@1", "Test Resource", "MyType");
			IntegerParameter iP = new IntegerParameter("nbOfBooks", "Number of Books", 33);
			res1.addParameter(BAG_ID, iP);
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.add(res1);
				tx.commitOnClose();
			}

			ResourceQuery<Resource> query = ResourceQuery.query("MyType");
			List<Selection> elementAndSelections = new ArrayList<>();
			elementAndSelections.add(new IdSelection("@1"));
			elementAndSelections.add(integerSelection(BAG_ID, "nbOfBooks", 33));
			query.and().with(elementAndSelections);

			InMemoryResourceQueryVisitor resourceQuery = new InMemoryResourceQueryVisitor();
			InMemoryQuery<Resource, Resource> inMemoryQuery = resourceQuery.visit(query);
			List<Resource> result;
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				result = inMemoryQuery.doQuery(tx, tx.getResourceMap());
			}
			assertEquals(1, result.size());
			assertEquals("@1", result.get(0).getId());
		});
	}

	@Test
	public void shouldQueryOrderWithParamValue() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Order o1 = createOrder("@1", "Test Order", "MyType");
			IntegerParameter iP = new IntegerParameter("nbOfBooks", "Number of Books", 33);
			o1.addParameter(BAG_ID, iP);
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.add(o1);
				tx.commitOnClose();
			}

			OrderQuery<Order> query = OrderQuery.query("MyType");
			List<Selection> elementAndSelections = new ArrayList<>();
			elementAndSelections.add(new IdSelection("@1"));
			elementAndSelections.add(integerSelection(BAG_ID, "nbOfBooks", 33));
			query.and().with(elementAndSelections);

			InMemoryOrderQueryVisitor orderQuery = new InMemoryOrderQueryVisitor();
			InMemoryQuery<Order, Order> inMemoryQuery = orderQuery.visit(query);
			List<Order> result;
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				result = inMemoryQuery.doQuery(tx, tx.getOrderMap());
			}
			assertEquals(1, result.size());
			assertEquals("@1", result.get(0).getId());
		});
	}

	@Test
	public void shouldQueryActivityWithParamValue() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Activity a1 = createActivity("@1", "Test Activity", "MyType", TimeOrdering.SERIES);
			IntegerParameter iP = new IntegerParameter("nbOfBooks", "Number of Books", 33);
			a1.addParameter(BAG_ID, iP);
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.add(a1);
				tx.commitOnClose();
			}

			ActivityQuery<Activity> query = ActivityQuery.query("MyType");
			List<Selection> elementAndSelections = new ArrayList<>();
			elementAndSelections.add(new IdSelection("@1"));
			elementAndSelections.add(integerSelection(BAG_ID, "nbOfBooks", 33));
			query.and().with(elementAndSelections);

			InMemoryActivityQueryVisitor orderQuery = new InMemoryActivityQueryVisitor();
			InMemoryQuery<Activity, Activity> inMemoryQuery = orderQuery.visit(query);
			List<Activity> result;
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				result = inMemoryQuery.doQuery(tx, tx.getActivityMap());
			}
			assertEquals(1, result.size());
			assertEquals("@1", result.get(0).getId());
		});
	}

	@Test
	public void shouldQueryContainsString() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Resource res1 = createResource("@1", "Test Resource", "MyType");
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.getResourceMap().add(tx, res1);
				tx.commitOnClose();
			}

			ResourceQuery<Resource> query = ResourceQuery.query("MyType");
			query.and().with(ParameterSelection
					.stringSelection(BAG_ID, PARAM_STRING_ID, "olch", StringMatchMode.CONTAINS_CASE_SENSITIVE));
			List<Resource> result;
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				result = tx.doQuery(query);
			}
			assertEquals(1, result.size());
			assertEquals("@1", result.get(0).getId());
		});
	}

	@Test
	public void shouldNotQueryContainsString() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Resource res1 = createResource("@1", "Test Resource", "MyType");
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.getResourceMap().add(tx, res1);
				tx.commitOnClose();
			}

			ResourceQuery<Resource> query = ResourceQuery.query("MyType");
			query.and().with(ParameterSelection
					.stringSelection(BAG_ID, PARAM_STRING_ID, "str", StringMatchMode.CONTAINS_CASE_SENSITIVE));
			List<Resource> result;
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				result = tx.doQuery(query);
			}
			assertEquals(0, result.size());
		});
	}

	@Test
	public void shouldQueryCaseInsensitiveString() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Resource res1 = createResource("@1", "Test Resource", "MyType");
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.getResourceMap().add(tx, res1);
				tx.commitOnClose();
			}

			ResourceQuery<Resource> query = ResourceQuery.query("MyType");
			query.and().with(ParameterSelection
					.stringSelection(BAG_ID, PARAM_STRING_ID, "strolch", StringMatchMode.EQUALS_CASE_INSENSITIVE));
			List<Resource> result;
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				result = tx.doQuery(query);
			}
			assertEquals(1, result.size());
			assertEquals("@1", result.get(0).getId());
		});
	}

	@Test
	public void shouldNotQueryCaseInsensitiveString() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Resource res1 = createResource("@1", "Test Resource", "MyType");
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.getResourceMap().add(tx, res1);
				tx.commitOnClose();
			}

			ResourceQuery<Resource> query = ResourceQuery.query("MyType");
			query.and().with(ParameterSelection
					.stringSelection(BAG_ID, PARAM_STRING_ID, "strolch", StringMatchMode.EQUALS_CASE_SENSITIVE));
			List<Resource> result;
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				result = tx.doQuery(query);
			}
			assertEquals(0, result.size());
		});
	}

	@Test
	public void shouldQueryNot() throws Exception {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_EMPTY_CONTAINER, agent -> {
			ComponentContainer container = agent.getContainer();

			Certificate certificate = login(container);

			Resource res1 = createResource("@1", "Test Resource", "MyType");
			Resource res2 = createResource("@2", "Test Resource", "MyType");
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test")) {
				tx.getResourceMap().add(tx, res1);
				tx.getResourceMap().add(tx, res2);
				tx.commitOnClose();
			}

			{
				ResourceQuery<Resource> query = ResourceQuery.query("MyType");
				query.not(new IdSelection("@1"));
				List<Resource> result;
				try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
						.openTx(certificate, "test")) {
					result = tx.doQuery(query);
				}
				assertEquals(1, result.size());
				assertEquals("@2", result.get(0).getId());
			}

			{
				ResourceQuery<Resource> query = ResourceQuery.query("MyType");
				query.not(new IdSelection("@2"));
				List<Resource> result;
				try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
						.openTx(certificate, "test")) {
					result = tx.doQuery(query);
				}
				assertEquals(1, result.size());
				assertEquals("@1", result.get(0).getId());
			}

			{
				ResourceQuery<Resource> query = ResourceQuery.query("MyType");
				query.not(new IdSelection("@1", "@2"));
				List<Resource> result;
				try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM)
						.openTx(certificate, "test")) {
					result = tx.doQuery(query);
				}
				assertEquals(0, result.size());
			}
		});
	}
}
