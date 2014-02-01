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
package li.strolch.runtime.configuration;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.helper.StringHelper;

public class ConfigurationSaxParser extends DefaultHandler {

	//private static final Logger logger = LoggerFactory.getLogger(ConfigurationSaxParser.class);

	private ConfigurationBuilder configurationBuilder;
	private LocatorBuilder locatorBuilder;
	private Deque<ElementHandler> delegateHandlers;

	public ConfigurationSaxParser() {
		this.locatorBuilder = new LocatorBuilder();
		this.delegateHandlers = new ArrayDeque<>();
		this.configurationBuilder = new ConfigurationBuilder();
	}

	public ConfigurationBuilder getConfigurationBuilder() {
		return this.configurationBuilder;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		this.locatorBuilder.append(qName);

		Locator locator = this.locatorBuilder.build();
		//logger.info("path: " + locator.toString()); //$NON-NLS-1$

		switch (locator.toString()) {

		case "StrolchConfiguration/Runtime": //$NON-NLS-1$
			RuntimeHandler runtimeHandler = new RuntimeHandler(this.configurationBuilder, locator);
			this.delegateHandlers.push(runtimeHandler);
			break;

		case "StrolchConfiguration/Runtime/Properties": //$NON-NLS-1$
			PropertiesHandler runtimePropertiesHandler = new PropertiesHandler(this.configurationBuilder, locator);
			this.delegateHandlers.push(runtimePropertiesHandler);
			this.configurationBuilder.setPropertyBuilder(this.configurationBuilder.runtimeBuilder());
			break;

		case "StrolchConfiguration/Component": //$NON-NLS-1$
			this.configurationBuilder.nextComponentBuilder();
			ComponentHandler componentHandler = new ComponentHandler(this.configurationBuilder, locator);
			this.delegateHandlers.push(componentHandler);
			break;

		case "StrolchConfiguration/Component/Properties": //$NON-NLS-1$
			PropertiesHandler componentPropertiesHandler = new PropertiesHandler(this.configurationBuilder, locator);
			this.delegateHandlers.push(componentPropertiesHandler);
			this.configurationBuilder.setPropertyBuilder(this.configurationBuilder.componentBuilder());
			break;

		default:
			if (!this.delegateHandlers.isEmpty())
				this.delegateHandlers.peek().startElement(uri, localName, qName, attributes);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (!this.delegateHandlers.isEmpty())
			this.delegateHandlers.peek().characters(ch, start, length);
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		Locator locator = this.locatorBuilder.build();
		//logger.info("path: " + locator.toString()); //$NON-NLS-1$

		switch (locator.toString()) {

		case "StrolchConfiguration/Runtime": //$NON-NLS-1$
			assertExpectedLocator(locator, this.delegateHandlers.pop().getLocator());
			break;

		case "StrolchConfiguration/Runtime/Properties": //$NON-NLS-1$
			assertExpectedLocator(locator, this.delegateHandlers.pop().getLocator());
			this.configurationBuilder.setPropertyBuilder(null);
			break;

		case "StrolchConfiguration/Component": //$NON-NLS-1$
			assertExpectedLocator(locator, this.delegateHandlers.pop().getLocator());
			break;

		case "StrolchConfiguration/Component/Properties": //$NON-NLS-1$
			assertExpectedLocator(locator, this.delegateHandlers.pop().getLocator());
			this.configurationBuilder.setPropertyBuilder(null);
			break;

		default:
			if (!this.delegateHandlers.isEmpty())
				this.delegateHandlers.peek().endElement(uri, localName, qName);
		}

		this.locatorBuilder.removeLast();
	}

	private void assertExpectedLocator(Locator expectedLocator, Locator actualLocator) {
		if (!expectedLocator.equals(actualLocator)) {
			String msg = "Locator mismatch. Expected {0}. Current: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, expectedLocator, actualLocator);
			throw new IllegalStateException(msg);
		}
	}

	public class ElementHandler extends DefaultHandler {
		protected final ConfigurationBuilder configurationBuilder;
		protected final Locator locator;
		protected StringBuilder valueBuffer;

		public ElementHandler(ConfigurationBuilder configurationBuilder, Locator locator) {
			this.configurationBuilder = configurationBuilder;
			this.locator = locator;
		}

		public Locator getLocator() {
			return this.locator;
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (this.valueBuffer != null)
				this.valueBuffer.append(ch, start, length);
		}
	}

	public class RuntimeHandler extends ElementHandler {

		public RuntimeHandler(ConfigurationBuilder configurationBuilder, Locator locator) {
			super(configurationBuilder, locator);
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			switch (qName) {
			case "applicationName": //$NON-NLS-1$
				this.valueBuffer = new StringBuilder();
				break;
			default:
				break;
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			switch (qName) {
			case "applicationName": //$NON-NLS-1$
				String applicationName = this.valueBuffer.toString();
				this.configurationBuilder.runtimeBuilder().setApplicationName(applicationName);
				this.valueBuffer = null;
				break;
			default:
				break;
			}
		}
	}

	public class ComponentHandler extends ElementHandler {

		public ComponentHandler(ConfigurationBuilder configurationBuilder, Locator locator) {
			super(configurationBuilder, locator);
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			switch (qName) {
			case "name": //$NON-NLS-1$
				this.valueBuffer = new StringBuilder();
				break;
			case "api": //$NON-NLS-1$
				this.valueBuffer = new StringBuilder();
				break;
			case "impl": //$NON-NLS-1$
				this.valueBuffer = new StringBuilder();
				break;
			case "depends": //$NON-NLS-1$
				this.valueBuffer = new StringBuilder();
				break;
			default:
				break;
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			switch (qName) {
			case "name": //$NON-NLS-1$
				String name = this.valueBuffer.toString();
				this.configurationBuilder.componentBuilder().setName(name);
				this.valueBuffer = null;
				break;
			case "api": //$NON-NLS-1$
				String api = this.valueBuffer.toString();
				this.configurationBuilder.componentBuilder().setApi(api);
				this.valueBuffer = null;
				break;
			case "impl": //$NON-NLS-1$
				String impl = this.valueBuffer.toString();
				this.configurationBuilder.componentBuilder().setImpl(impl);
				break;
			case "depends": //$NON-NLS-1$
				String depends = this.valueBuffer.toString();
				this.configurationBuilder.componentBuilder().addDependency(depends);
				break;
			default:
				break;
			}
		}

	}

	public class PropertiesHandler extends ElementHandler {

		public PropertiesHandler(ConfigurationBuilder configurationBuilder, Locator locator) {
			super(configurationBuilder, locator);
		}

		private String propertyName;

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			this.propertyName = qName;
			this.valueBuffer = new StringBuilder();
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			if (this.propertyName == null || !this.propertyName.equals(qName)) {
				String msg = "Previous tag '{0}' was not closed before new tag {1}!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.propertyName, qName);
				throw new IllegalStateException(msg);
			}

			String propertyValue = this.valueBuffer.toString().trim();
			this.configurationBuilder.getPropertyBuilder().addProperty(this.propertyName, propertyValue);
			this.propertyName = null;
			this.valueBuffer = null;
		}
	}

	public class ConfigurationBuilder {

		private RuntimeBuilder runtimeBuilder;
		private ComponentBuilder componentBuilder;
		private PropertyBuilder propertyBuilder;
		private List<ComponentBuilder> componentBuilders;

		public ConfigurationBuilder() {
			this.componentBuilders = new ArrayList<>();
		}

		public void setPropertyBuilder(PropertyBuilder propertyBuilder) {
			this.propertyBuilder = propertyBuilder;
		}

		public PropertyBuilder getPropertyBuilder() {
			return this.propertyBuilder;
		}

		public RuntimeBuilder runtimeBuilder() {
			if (this.runtimeBuilder == null)
				this.runtimeBuilder = new RuntimeBuilder();
			return this.runtimeBuilder;
		}

		public ComponentBuilder nextComponentBuilder() {
			this.componentBuilder = new ComponentBuilder();
			this.componentBuilders.add(this.componentBuilder);
			return this.componentBuilder;
		}

		public ComponentBuilder componentBuilder() {
			return this.componentBuilder;
		}

		public StrolchConfiguration build(File rootPathF) {

			RuntimeConfiguration runtimeConfiguration = this.runtimeBuilder.build(rootPathF);

			Map<String, ComponentConfiguration> configurationByComponent = new HashMap<>();
			for (ComponentBuilder componentBuilder : this.componentBuilders) {
				ComponentConfiguration componentConfiguration = componentBuilder.build(runtimeConfiguration);
				configurationByComponent.put(componentConfiguration.getName(), componentConfiguration);
			}

			StrolchConfiguration strolchConfiguration = new StrolchConfiguration(runtimeConfiguration,
					configurationByComponent);

			return strolchConfiguration;
		}
	}

	public abstract class PropertyBuilder {
		private Map<String, String> properties;

		public PropertyBuilder() {
			this.properties = new HashMap<>();
		}

		public void addProperty(String key, String value) {
			if (StringHelper.isEmpty(key))
				throw new IllegalStateException("Key is empty!"); //$NON-NLS-1$
			this.properties.put(key, value);
		}

		public Map<String, String> getProperties() {
			return this.properties;
		}
	}

	public class RuntimeBuilder extends PropertyBuilder {

		private String applicationName;

		public String getApplicationName() {
			return this.applicationName;
		}

		public RuntimeConfiguration build(File rootPathF) {
			RuntimeConfiguration configuration = new RuntimeConfiguration(this.applicationName, getProperties(),
					rootPathF);
			return configuration;
		}

		public RuntimeBuilder setApplicationName(String applicationName) {
			this.applicationName = applicationName;
			return this;
		}
	}

	public class ComponentBuilder extends PropertyBuilder {

		private String name;
		private String api;
		private String impl;
		private Set<String> dependencies;

		public ComponentBuilder() {
			this.dependencies = new HashSet<>();
		}

		public ComponentConfiguration build(RuntimeConfiguration runtimeConfiguration) {
			ComponentConfiguration componentConfiguration = new ComponentConfiguration(runtimeConfiguration, this.name,
					getProperties(), this.api, this.impl, this.dependencies);
			return componentConfiguration;
		}

		public String getName() {
			return this.name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getApi() {
			return this.api;
		}

		public void setApi(String api) {
			this.api = api;
		}

		public String getImpl() {
			return this.impl;
		}

		public void setImpl(String impl) {
			this.impl = impl;
		}

		public Set<String> getDependencies() {
			return this.dependencies;
		}

		public void addDependency(String dependency) {
			this.dependencies.add(dependency);
		}
	}
}