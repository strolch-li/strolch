package li.strolch.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.ComponentController;
import li.strolch.runtime.component.ComponentDependencyAnalyzer;
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ConfigurationParser;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

@SuppressWarnings("nls")
public class ControllerDependencyTest {

	@Rule
	public ExpectedException thrown = ExpectedException.none();

	//  
	//  
	//         5
	//         v
	//   2 <-- 11 ---> 10
	//        ^  v     ^
	//        |  9     3
	//        |   ^   /
	//        |    | v
	//       7 --> 8
	//
	//  a -> b : Upstream dependency for a is b 
	//         : Downstream dependency for b is a
	//

	private ComponentContainer container;
	private StrolchComponent com2;
	private ComponentController con2;
	private StrolchComponent com5;
	private ComponentController con5;
	private StrolchComponent com11;
	private ComponentController con11;
	private StrolchComponent com10;
	private ComponentController con10;
	private StrolchComponent com9;
	private ComponentController con9;
	private StrolchComponent com7;
	private ComponentController con7;
	private StrolchComponent com8;
	private ComponentController con8;
	private StrolchComponent com3;
	private ComponentController con3;

	private StrolchConfiguration strolchConfiguration;
	private Map<String, ComponentController> controllerMap;

	@Before
	public void setupModel() {

		this.container = new ComponentContainer();

		this.com2 = new StrolchComponent(this.container, "2");
		this.con2 = new ComponentController(this.com2);
		this.com5 = new StrolchComponent(this.container, "5");
		this.con5 = new ComponentController(this.com5);
		this.com11 = new StrolchComponent(this.container, "11");
		this.con11 = new ComponentController(this.com11);
		this.com10 = new StrolchComponent(this.container, "10");
		this.con10 = new ComponentController(this.com10);
		this.com9 = new StrolchComponent(this.container, "9");
		this.con9 = new ComponentController(this.com9);
		this.com7 = new StrolchComponent(this.container, "7");
		this.con7 = new ComponentController(this.com7);
		this.com8 = new StrolchComponent(this.container, "8");
		this.con8 = new ComponentController(this.com8);
		this.com3 = new StrolchComponent(this.container, "3");
		this.con3 = new ComponentController(this.com3);

		this.con5.addUpstreamDependency(this.con11);

		this.con11.addUpstreamDependency(this.con2);
		this.con11.addUpstreamDependency(this.con10);
		this.con11.addUpstreamDependency(this.con9);

		this.con7.addUpstreamDependency(this.con11);
		this.con7.addUpstreamDependency(this.con8);

		this.con8.addUpstreamDependency(this.con9);

		this.con3.addUpstreamDependency(this.con8);
		this.con3.addUpstreamDependency(this.con10);

		File rootPathF = new File("src/test/resources/configtest");
		this.strolchConfiguration = ConfigurationParser.parseConfiguration(rootPathF);
		this.controllerMap = new HashMap<>();
	}

	private void assertModel() {

		assertTrue(this.con5.hasUpstreamDependencies());
		assertFalse(this.con5.hasDownstreamDependencies());
		assertEquals(1, this.con5.getUpstreamDependencies().size());
		assertEquals(0, this.con5.getDownstreamDependencies().size());

		assertTrue(this.con11.hasUpstreamDependencies());
		assertTrue(this.con11.hasDownstreamDependencies());
		assertEquals(3, this.con11.getUpstreamDependencies().size());
		assertEquals(2, this.con11.getDownstreamDependencies().size());

		assertFalse(this.con2.hasUpstreamDependencies());
		assertTrue(this.con2.hasDownstreamDependencies());
		assertEquals(0, this.con2.getUpstreamDependencies().size());
		assertEquals(1, this.con2.getDownstreamDependencies().size());

		assertTrue(this.con7.hasUpstreamDependencies());
		assertFalse(this.con7.hasDownstreamDependencies());
		assertEquals(2, this.con7.getUpstreamDependencies().size());
		assertEquals(0, this.con7.getDownstreamDependencies().size());

		assertTrue(this.con8.hasUpstreamDependencies());
		assertTrue(this.con8.hasDownstreamDependencies());
		assertEquals(1, this.con8.getUpstreamDependencies().size());
		assertEquals(2, this.con8.getDownstreamDependencies().size());

		assertTrue(this.con3.hasUpstreamDependencies());
		assertFalse(this.con3.hasDownstreamDependencies());
		assertEquals(2, this.con3.getUpstreamDependencies().size());
		assertEquals(0, this.con3.getDownstreamDependencies().size());

		assertFalse(this.con10.hasUpstreamDependencies());
		assertTrue(this.con10.hasDownstreamDependencies());
		assertEquals(0, this.con10.getUpstreamDependencies().size());
		assertEquals(2, this.con10.getDownstreamDependencies().size());

		assertFalse(this.con9.hasUpstreamDependencies());
		assertTrue(this.con9.hasDownstreamDependencies());
		assertEquals(0, this.con9.getUpstreamDependencies().size());
		assertEquals(2, this.con9.getDownstreamDependencies().size());

		// transitive upstream
		assertTrue(this.con7.hasTransitiveUpstreamDependency(this.con11));
		assertTrue(this.con5.hasTransitiveUpstreamDependency(this.con11));
		assertTrue(this.con11.hasTransitiveUpstreamDependency(this.con2));
		assertTrue(this.con11.hasTransitiveUpstreamDependency(this.con10));
		assertFalse(this.con11.hasTransitiveUpstreamDependency(this.con3));
		assertTrue(this.con8.hasTransitiveUpstreamDependency(this.con9));

		assertTrue(this.con5.hasTransitiveUpstreamDependency(this.con10));
		assertTrue(this.con5.hasTransitiveUpstreamDependency(this.con2));
		assertTrue(this.con5.hasTransitiveUpstreamDependency(this.con9));
		assertTrue(this.con7.hasTransitiveUpstreamDependency(this.con2));
		assertTrue(this.con7.hasTransitiveUpstreamDependency(this.con10));
		assertTrue(this.con7.hasTransitiveUpstreamDependency(this.con9));
		assertTrue(this.con3.hasTransitiveUpstreamDependency(this.con9));
		assertFalse(this.con9.hasTransitiveUpstreamDependency(this.con10));
		assertFalse(this.con9.hasTransitiveUpstreamDependency(this.con11));
		assertFalse(this.con9.hasTransitiveUpstreamDependency(this.con8));
		assertFalse(this.con9.hasTransitiveUpstreamDependency(this.con7));

		// transitive downstream
		assertFalse(this.con5.hasTransitiveDownstreamDependency(this.con8));
		assertFalse(this.con5.hasTransitiveDownstreamDependency(this.con3));
		assertFalse(this.con5.hasTransitiveDownstreamDependency(this.con7));
		assertFalse(this.con9.hasTransitiveDownstreamDependency(this.con10));
		assertTrue(this.con9.hasTransitiveDownstreamDependency(this.con3));
		assertTrue(this.con9.hasTransitiveDownstreamDependency(this.con7));

	}

	@Test
	public void testModel() {
		assertModel();
	}

	@Test
	public void shouldBreakModel1() {
		this.thrown.expect(StrolchConfigurationException.class);
		assertModel();
		this.con2.addUpstreamDependency(this.con7);
	}

	@Test
	public void shouldBreakModel2() {
		this.thrown.expect(StrolchConfigurationException.class);
		assertModel();
		this.con3.addUpstreamDependency(this.con3);
	}

	@Test
	public void shouldBreakModel3() {
		this.thrown.expect(StrolchConfigurationException.class);
		assertModel();
		this.con9.addUpstreamDependency(this.con3);
	}

	@Test
	public void shouldBreakModel4() {
		this.thrown.expect(StrolchConfigurationException.class);
		assertModel();
		this.con7.addUpstreamDependency(this.con10);
	}

	@Test
	public void shouldNotBreakModel1() {
		assertModel();
		this.con11.addUpstreamDependency(this.con8);
	}

	@Test
	public void shouldNotBreakModel2() {
		assertModel();
		this.con9.addUpstreamDependency(this.con10);
	}

	@Test
	public void shouldNotBreakModel3() {
		assertModel();
		this.con11.addUpstreamDependency(this.con8);
	}

	@Test
	public void shouldNotBreakModel4() {
		assertModel();
		this.con8.addUpstreamDependency(this.con11);
	}

	//         11
	//    ^   ^  ^
	//    7   9 < 8
	//    
	//   (7, 8) => (11)

	@Test
	@Ignore
	public void shouldCollectUpstreamDependencies() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		this.con8.addUpstreamDependency(this.con11);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.con7);
		controllers.add(this.con8);
		
		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.findDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.con11));
	}

	@Test
	public void shouldAddDepedencies() {

		ComponentContainer container = new ComponentContainer();
		StrolchComponent component = new StrolchComponent(container, "1"); //$NON-NLS-1$
		ComponentController controller = new ComponentController(component);

		StrolchComponent upstreamDependencyComp1 = new StrolchComponent(container, "1"); //$NON-NLS-1$
		ComponentController upstreamDependency1 = new ComponentController(upstreamDependencyComp1);

		StrolchComponent upstreamDependencyComp2 = new StrolchComponent(container, "1"); //$NON-NLS-1$
		ComponentController upstreamDependency2 = new ComponentController(upstreamDependencyComp2);

		StrolchComponent transitiveUpstreamDependencyComp2 = new StrolchComponent(container, "1"); //$NON-NLS-1$
		ComponentController transitiveUpstreamDependency2 = new ComponentController(transitiveUpstreamDependencyComp2);

		controller.addUpstreamDependency(upstreamDependency1);
		controller.addUpstreamDependency(upstreamDependency2);
		upstreamDependency1.addUpstreamDependency(transitiveUpstreamDependency2);

		assertTrue(controller.hasUpstreamDependencies());
		assertFalse(controller.hasDownstreamDependencies());
		assertTrue(controller.hasUpstreamDependency(upstreamDependency1));
		assertTrue(controller.hasUpstreamDependency(upstreamDependency2));

		assertTrue(upstreamDependency1.hasDownstreamDependency(controller));
		assertTrue(upstreamDependency2.hasDownstreamDependency(controller));

		assertTrue(upstreamDependency1.hasUpstreamDependencies());
		assertTrue(upstreamDependency1.hasUpstreamDependency(transitiveUpstreamDependency2));
		assertTrue(transitiveUpstreamDependency2.hasDownstreamDependencies());

		assertTrue(controller.hasTransitiveUpstreamDependency(transitiveUpstreamDependency2));
		assertTrue(transitiveUpstreamDependency2.hasTransitiveDownstreamDependency(controller));
	}

}
