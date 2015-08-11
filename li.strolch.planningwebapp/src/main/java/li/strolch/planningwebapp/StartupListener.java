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
package li.strolch.planningwebapp;

import java.io.File;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.runtime.configuration.StrolchEnvironment;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@WebListener
@SuppressWarnings("nls")
public class StartupListener implements ServletContextListener {

	private static final Logger logger = LoggerFactory.getLogger(StartupListener.class);
	private StrolchAgent agent;

	@Override
	public void contextInitialized(ServletContextEvent sce) {

		String realPath = sce.getServletContext().getRealPath("/WEB-INF");

		File pathF = new File(realPath);
		String environment = StrolchEnvironment.getEnvironmentFromEnvProperties(pathF);
		logger.info("Starting Agent...");
		this.agent = new StrolchAgent();
		this.agent.setup(environment, pathF);
		this.agent.initialize();
		this.agent.start();
		logger.info("Agent started.");
	}

	@Override
	public void contextDestroyed(ServletContextEvent sce) {
		if (this.agent != null) {
			this.agent.stop();
			this.agent.destroy();
		}
		logger.info("Agent destroyed");
	}
}
