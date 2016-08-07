package li.strolch.runtime.query.inmemory;

import static li.strolch.model.query.ParameterSelection.booleanSelection;
import static li.strolch.model.query.ParameterSelection.floatListSelection;
import static li.strolch.model.query.ParameterSelection.floatSelection;
import static li.strolch.model.query.ParameterSelection.integerListSelection;
import static li.strolch.model.query.ParameterSelection.longListSelection;
import static li.strolch.model.query.ParameterSelection.stringListSelection;
import static li.strolch.model.query.ParameterSelection.stringSelection;
import static li.strolch.utils.StringMatchMode.ci;
import static li.strolch.utils.StringMatchMode.es;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.Version;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.FloatListParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerListParameter;
import li.strolch.model.parameter.LongListParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.inmemory.InMemoryResourceDao;

public class InMemoryResourceQueryTest {

	protected InMemoryResourceDao daoInstance() {
		return new InMemoryResourceDao(false);
	}

	@Test
	public void shouldQueryById() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> resourceQuery = ResourceQuery.query("MyType1");
		resourceQuery.with(new IdSelection("@1"));

		List<Resource> result = dao.doQuery(resourceQuery);
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryByIdOr() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> resourceQuery = ResourceQuery.query("MyType2");
		resourceQuery.or().with(new IdSelection("@3"), new IdSelection("@4"));

		List<Resource> result = dao.doQuery(resourceQuery);
		assertEquals(2, result.size());
		assertEquals("@3", result.get(0).getId());
		assertEquals("@4", result.get(1).getId());
	}

	@Test
	public void shouldQueryByIdAnd() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> resourceQuery = ResourceQuery.query("MyType2");
		resourceQuery.and().with(new IdSelection("@3"), new NameSelection("Res 3", es()));

		List<Resource> result = dao.doQuery(resourceQuery);
		assertEquals(1, result.size());
		assertEquals("@3", result.get(0).getId());
	}

	@Test
	public void shouldNotQueryByIdAnd() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> resourceQuery = ResourceQuery.query("MyType1");
		resourceQuery.and().with(new IdSelection("@3"), new NameSelection("@4", es()));

		List<Resource> result = dao.doQuery(resourceQuery);
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByParameter() {

		List<Resource> resources = getResources();
		resources.add(getBallResource());
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> ballQuery = ResourceQuery.query("Ball");
		ballQuery.and().with(
				//
				stringSelection("parameters", "color", "red", es()),
				booleanSelection("parameters", "forChildren", true), floatSelection("parameters", "diameter", 22.0));

		List<Resource> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByListParameter() {

		List<Resource> resources = getResources();
		resources.add(getBallResource());
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> ballQuery;
		List<Resource> result;

		// string list
		{
			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a", "z")));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a")));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("c", "b", "a")));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// integer list
		{
			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1, 5)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(3, 2, 1)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// float list
		{
			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0, 8.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(6.2, 5.1, 4.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// long list
		{
			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L, 11L)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = ResourceQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(10L, 9L, 8L)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldQueryByNullParameter1() {
		List<Resource> resources = getResources();
		resources.add(getBallResource());
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> ballQuery = ResourceQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "color"));

		List<Resource> result = dao.doQuery(ballQuery);
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByNullParameter2() {
		List<Resource> resources = getResources();
		resources.add(getBallResource());
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> ballQuery = ResourceQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "weight"));

		List<Resource> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByNullParameter3() {
		List<Resource> resources = getResources();
		resources.add(getBallResource());
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> ballQuery = ResourceQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "weight"));

		List<Resource> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByName() {

		List<Resource> resources = getResources();
		resources.add(getBallResource());
		InMemoryResourceDao dao = daoInstance();
		dao.saveAll(resources);

		ResourceQuery<Resource> ballQuery = ResourceQuery.query("Ball");
		ballQuery.with(new NameSelection("ball ", ci()));

		List<Resource> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	private Resource getBallResource() {
		Resource res1 = new Resource("childrensBall", "Ball 1", "Ball");
		Version.setInitialVersionFor(res1, "test");
		ParameterBag bag = new ParameterBag("parameters", "Ball Details", "Parameters");
		bag.addParameter(new StringParameter("color", "Color", "red"));
		bag.addParameter(new BooleanParameter("forChildren", "Color", true));
		bag.addParameter(new FloatParameter("diameter", "Color", 22.0));
		bag.addParameter(
				new StringListParameter("stringListValues", "List of String Values", Arrays.asList("a", "b", "c")));
		bag.addParameter(new IntegerListParameter("intListValues", "List of Integer Values", Arrays.asList(1, 2, 3)));
		bag.addParameter(
				new FloatListParameter("floatListValues", "List of Float Values", Arrays.asList(4.0, 5.1, 6.2)));
		bag.addParameter(new LongListParameter("longListValues", "List of Long Values", Arrays.asList(8L, 9L, 10L)));
		res1.addParameterBag(bag);
		return res1;
	}

	private List<Resource> getResources() {
		Resource res1 = ModelGenerator.createResource("@1", "Res 1", "MyType1");
		Resource res2 = ModelGenerator.createResource("@2", "Res 2", "MyType1");
		Resource res3 = ModelGenerator.createResource("@3", "Res 3", "MyType2");
		Resource res4 = ModelGenerator.createResource("@4", "Res 4", "MyType2");
		Resource res5 = ModelGenerator.createResource("@5", "Res 5", "MyType3");
		Resource res6 = ModelGenerator.createResource("@6", "Res 6", "MyType3");
		List<Resource> resources = new ArrayList<>();
		resources.add(res1);
		resources.add(res2);
		resources.add(res3);
		resources.add(res4);
		resources.add(res5);
		resources.add(res6);

		for (Resource resource : resources) {
			Version.setInitialVersionFor(resource, "test");
		}

		return resources;
	}
}
