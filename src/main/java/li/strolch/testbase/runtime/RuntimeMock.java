package li.strolch.testbase.runtime;

import java.io.File;
import java.text.MessageFormat;

import li.strolch.runtime.configuration.RuntimeConfiguration;
import ch.eitchnet.utils.helper.FileHelper;

public class RuntimeMock {

	private static final String TARGET = "target"; //$NON-NLS-1$

	public static void mockRuntime(File rootPathF, File configSrc) {

		if (!rootPathF.getParentFile().getName().equals(TARGET)) {
			String msg = "Mocking path must be in a maven target: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		if (!configSrc.isDirectory() || !configSrc.canRead()) {
			String msg = "Could not find config source in: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configSrc.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		if (rootPathF.exists()) {
			if (!FileHelper.deleteFile(rootPathF, true)) {
				String msg = "Failed to delete {0}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, rootPathF.getAbsolutePath());
				throw new RuntimeException(msg);
			}
		}

		if (!rootPathF.mkdirs()) {
			String msg = "Failed to create {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, rootPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		File configPathF = new File(rootPathF, RuntimeConfiguration.PATH_CONFIG);
		configPathF.mkdir();

		if (!FileHelper.copy(configSrc.listFiles(), configPathF, false)) {
			String msg = "Failed to copy source configs from {0} to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, configSrc.getAbsolutePath(), configPathF.getAbsolutePath());
			throw new RuntimeException(msg);
		}
//
//		// initialize a runtime configuration
//		Map<String, String> runtimeConfigurationValues = new HashMap<>();
//		RuntimeConfiguration runtimeConfiguration = new RuntimeConfiguration("RuntimeMock", runtimeConfigurationValues, //$NON-NLS-1$
//				rootPathF);
//
//		Map<String, ComponentConfiguration> configurationByComponent = new HashMap<>();
//		StrolchConfiguration configuration = new StrolchConfiguration(runtimeConfiguration, configurationByComponent);
//
//		// initialize the component configuration
//		for (ComponentMock componentMock : runtimeMocks) {
//			//configurationByComponent.put(key, value)
//			componentMock.mock(configuration);
//		}
	}
}
