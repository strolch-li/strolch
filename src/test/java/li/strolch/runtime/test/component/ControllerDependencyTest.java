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
package li.strolch.runtime.test.component;

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
	//
	//        A
	//       ^ ^
	//      B   C
	//     ^
	//     D 
	//
	//
	//        A1
	//      ^   ^
	//     B1 > C1
	//     ^
	//    D1
	//
	//
	//    +-> A2
	//    |   ^   
	//    |  B2 > C2
	//    |  ^
	//     D2
	//
	//    
	//
	//  a -> b : Upstream dependency for a is b 
	//         : Downstream dependency for b is a
	//

	private ComponentContainer container;
	private ComponentController con2;
	private ComponentController con5;
	private ComponentController con11;
	private ComponentController con10;
	private ComponentController con9;
	private ComponentController con7;
	private ComponentController con8;
	private ComponentController con3;
	private ComponentController conA;
	private ComponentController conB;
	private ComponentController conC;
	private ComponentController conD;
	private ComponentController conA1;
	private ComponentController conB1;
	private ComponentController conC1;
	private ComponentController conD1;
	private ComponentController conA2;
	private ComponentController conB2;
	private ComponentController conC2;
	private ComponentController conD2;

	private StrolchConfiguration strolchConfiguration;
	private Map<String, ComponentController> controllerMap;

	@Before
	public void setupModel() {

		this.container = new ComponentContainer();

		this.con2 = new ComponentController(new StrolchComponent(this.container, "2"));
		this.con5 = new ComponentController(new StrolchComponent(this.container, "5"));
		this.con11 = new ComponentController(new StrolchComponent(this.container, "11"));
		this.con10 = new ComponentController(new StrolchComponent(this.container, "10"));
		this.con9 = new ComponentController(new StrolchComponent(this.container, "9"));
		this.con7 = new ComponentController(new StrolchComponent(this.container, "7"));
		this.con8 = new ComponentController(new StrolchComponent(this.container, "8"));
		this.con3 = new ComponentController(new StrolchComponent(this.container, "3"));

		this.con5.addUpstreamDependency(this.con11);

		this.con11.addUpstreamDependency(this.con2);
		this.con11.addUpstreamDependency(this.con10);
		this.con11.addUpstreamDependency(this.con9);

		this.con7.addUpstreamDependency(this.con11);
		this.con7.addUpstreamDependency(this.con8);

		this.con8.addUpstreamDependency(this.con9);

		this.con3.addUpstreamDependency(this.con8);
		this.con3.addUpstreamDependency(this.con10);

		//

		this.conA = new ComponentController(new StrolchComponent(this.container, "A"));
		this.conB = new ComponentController(new StrolchComponent(this.container, "B"));
		this.conC = new ComponentController(new StrolchComponent(this.container, "C"));
		this.conD = new ComponentController(new StrolchComponent(this.container, "D"));

		this.conB.addUpstreamDependency(this.conA);
		this.conC.addUpstreamDependency(this.conA);
		this.conD.addUpstreamDependency(this.conB);

		//

		this.conA1 = new ComponentController(new StrolchComponent(this.container, "A1"));
		this.conB1 = new ComponentController(new StrolchComponent(this.container, "B1"));
		this.conC1 = new ComponentController(new StrolchComponent(this.container, "C1"));
		this.conD1 = new ComponentController(new StrolchComponent(this.container, "D1"));

		this.conB1.addUpstreamDependency(this.conA1);
		this.conB1.addUpstreamDependency(this.conC1);
		this.conC1.addUpstreamDependency(this.conA1);
		this.conD1.addUpstreamDependency(this.conB1);

		//

		this.conA2 = new ComponentController(new StrolchComponent(this.container, "A2"));
		this.conB2 = new ComponentController(new StrolchComponent(this.container, "B2"));
		this.conC2 = new ComponentController(new StrolchComponent(this.container, "C2"));
		this.conD2 = new ComponentController(new StrolchComponent(this.container, "D2"));

		this.conB2.addUpstreamDependency(this.conA2);
		this.conB2.addUpstreamDependency(this.conC2);
		this.conD2.addUpstreamDependency(this.conB2);
		this.conD2.addUpstreamDependency(this.conA2);

		//

		File rootPathF = new File("src/test/resources/configtest");
		this.strolchConfiguration = ConfigurationParser.parseConfiguration(rootPathF);

		this.controllerMap = new HashMap<>();

		this.controllerMap.put("2", this.con2);
		this.controllerMap.put("3", this.con3);
		this.controllerMap.put("5", this.con5);
		this.controllerMap.put("7", this.con7);
		this.controllerMap.put("8", this.con8);
		this.controllerMap.put("9", this.con9);
		this.controllerMap.put("10", this.con10);
		this.controllerMap.put("11", this.con11);
		this.controllerMap.put("A", this.conA);
		this.controllerMap.put("B", this.conB);
		this.controllerMap.put("C", this.conC);
		this.controllerMap.put("D", this.conD);
		this.controllerMap.put("A1", this.conA1);
		this.controllerMap.put("B1", this.conB1);
		this.controllerMap.put("C1", this.conC1);
		this.controllerMap.put("D1", this.conD1);
		this.controllerMap.put("A2", this.conA2);
		this.controllerMap.put("B2", this.conB2);
		this.controllerMap.put("C2", this.conC2);
		this.controllerMap.put("D2", this.conD2);

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

		//

		assertFalse(this.conA.hasUpstreamDependencies());
		assertTrue(this.conA.hasDownstreamDependencies());
		assertEquals(0, this.conA.getUpstreamDependencies().size());
		assertEquals(2, this.conA.getDownstreamDependencies().size());

		assertTrue(this.conB.hasUpstreamDependencies());
		assertTrue(this.conB.hasDownstreamDependencies());
		assertEquals(1, this.conB.getUpstreamDependencies().size());
		assertEquals(1, this.conB.getDownstreamDependencies().size());

		assertTrue(this.conC.hasUpstreamDependencies());
		assertFalse(this.conC.hasDownstreamDependencies());
		assertEquals(1, this.conC.getUpstreamDependencies().size());
		assertEquals(0, this.conC.getDownstreamDependencies().size());

		assertTrue(this.conD.hasUpstreamDependencies());
		assertFalse(this.conD.hasDownstreamDependencies());
		assertEquals(1, this.conD.getUpstreamDependencies().size());
		assertEquals(0, this.conD.getDownstreamDependencies().size());

		//

		assertFalse(this.conA1.hasUpstreamDependencies());
		assertTrue(this.conA1.hasDownstreamDependencies());
		assertEquals(0, this.conA1.getUpstreamDependencies().size());
		assertEquals(2, this.conA1.getDownstreamDependencies().size());

		assertTrue(this.conB1.hasUpstreamDependencies());
		assertTrue(this.conB1.hasDownstreamDependencies());
		assertEquals(2, this.conB1.getUpstreamDependencies().size());
		assertEquals(1, this.conB1.getDownstreamDependencies().size());

		assertTrue(this.conC1.hasUpstreamDependencies());
		assertTrue(this.conC1.hasDownstreamDependencies());
		assertEquals(1, this.conC1.getUpstreamDependencies().size());
		assertEquals(1, this.conC1.getDownstreamDependencies().size());

		assertTrue(this.conD1.hasUpstreamDependencies());
		assertFalse(this.conD1.hasDownstreamDependencies());
		assertEquals(1, this.conD1.getUpstreamDependencies().size());
		assertEquals(0, this.conD1.getDownstreamDependencies().size());
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

	@Test
	public void shouldNotBreakModel() {
		assertModel();
		this.con11.addUpstreamDependency(this.con3);
	}

	@Test
	public void shouldCollectUpstreamDependencies1() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conB);
		controllers.add(this.conC);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA));
	}

	@Test
	public void shouldCollectUpstreamDependencies2() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conD);
		controllers.add(this.conC);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA));
	}

	@Test
	public void shouldCollectUpstreamDependencies3_() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conB);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA));
	}

	//
	//        A
	//       ^ ^
	//      B   C
	//     ^
	//     D 
	//

	@Test
	public void shouldCollectUpstreamDependencies3() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conB1);
		controllers.add(this.conC1);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA1));
	}

	@Test
	public void shouldCollectUpstreamDependencies4() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conD1);
		controllers.add(this.conC1);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA1));
	}

	@Test
	public void shouldCollectUpstreamDependencies5() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conB1);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA1));
	}

	//
	//        A1
	//      ^   ^
	//     B1 > C1
	//     ^
	//    D1
	//

	@Test
	public void shouldCollectUpstreamDependencies6() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conB2);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(2, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA2));
		assertTrue(directUpstreamDependencies.contains(this.conC2));
	}

	@Test
	public void shouldCollectUpstreamDependencies7() {
		assertModel();

		ComponentDependencyAnalyzer dependencyAnalyzer = new ComponentDependencyAnalyzer(this.strolchConfiguration,
				this.controllerMap);

		Set<ComponentController> controllers = new HashSet<>();
		controllers.add(this.conD2);

		Set<ComponentController> directUpstreamDependencies = dependencyAnalyzer
				.collectDirectUpstreamDependencies(controllers);

		assertEquals(1, directUpstreamDependencies.size());
		assertTrue(directUpstreamDependencies.contains(this.conA2));
	}

	//    +-> A2
	//    |   ^   
	//    |  B2 > C2
	//    |  ^
	//     D2
	//

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
