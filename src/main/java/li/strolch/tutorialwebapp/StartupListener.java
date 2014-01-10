package li.strolch.tutorialwebapp;

import java.io.File;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import li.strolch.agent.api.StrolchAgent;

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
		logger.info("Starting Agent...");
		this.agent = new StrolchAgent();
		this.agent.setup(pathF);
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
