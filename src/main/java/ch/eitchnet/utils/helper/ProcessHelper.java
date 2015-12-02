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
package ch.eitchnet.utils.helper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ProcessHelper {

	private static final Logger logger = LoggerFactory.getLogger(ProcessHelper.class);

	public static ProcessResult runCommand(String command) {
		final StringBuffer sb = new StringBuffer();
		sb.append("=====================================\n"); //$NON-NLS-1$
		try {

			final Process process = Runtime.getRuntime().exec(command);
			final int[] returnValue = new int[1];

			try (final BufferedReader errorStream = new BufferedReader(new InputStreamReader(process.getErrorStream()));
					final BufferedReader inputStream = new BufferedReader(
							new InputStreamReader(process.getInputStream()));) {

				Thread errorIn = new Thread("errorIn") { //$NON-NLS-1$
					@Override
					public void run() {
						readStream(sb, "[ERROR] ", errorStream); //$NON-NLS-1$
					}
				};
				errorIn.start();

				Thread infoIn = new Thread("infoIn") { //$NON-NLS-1$
					@Override
					public void run() {
						readStream(sb, "[INFO] ", inputStream); //$NON-NLS-1$
					}
				};
				infoIn.start();

				returnValue[0] = process.waitFor();

				errorIn.join(100l);
				infoIn.join(100l);
				sb.append("=====================================\n"); //$NON-NLS-1$
			}
			return new ProcessResult(returnValue[0], sb.toString(), null);

		} catch (IOException e) {
			String msg = MessageFormat.format("Failed to perform command: {0}", e.getMessage()); //$NON-NLS-1$
			throw new RuntimeException(msg, e);
		} catch (InterruptedException e) {
			logger.error("Interrupted!"); //$NON-NLS-1$
			sb.append("[FATAL] Interrupted"); //$NON-NLS-1$
			return new ProcessResult(-1, sb.toString(), e);
		}
	}

	public static ProcessResult runCommand(File workingDirectory, String... commandAndArgs) {

		if (!workingDirectory.isDirectory()) {
			String msg = "Working directory does not exist or is not a directory at {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, workingDirectory.getAbsolutePath());
			throw new RuntimeException(msg);
		}
		if (commandAndArgs == null || commandAndArgs.length == 0)
			throw new RuntimeException("No command passed!"); //$NON-NLS-1$

		final StringBuffer sb = new StringBuffer();
		sb.append("=====================================\n"); //$NON-NLS-1$
		try {

			ProcessBuilder processBuilder = new ProcessBuilder(commandAndArgs);
			processBuilder.environment();
			processBuilder.directory(workingDirectory);

			final Process process = processBuilder.start();
			int[] returnValue = new int[1];

			try (final BufferedReader errorStream = new BufferedReader(new InputStreamReader(process.getErrorStream()));
					final BufferedReader inputStream = new BufferedReader(
							new InputStreamReader(process.getInputStream()));) {

				Thread errorIn = new Thread("errorIn") { //$NON-NLS-1$
					@Override
					public void run() {
						readStream(sb, "[ERROR] ", errorStream); //$NON-NLS-1$
					}
				};
				errorIn.start();

				Thread infoIn = new Thread("infoIn") { //$NON-NLS-1$
					@Override
					public void run() {
						readStream(sb, "[INFO] ", inputStream); //$NON-NLS-1$
					}
				};
				infoIn.start();

				returnValue[0] = process.waitFor();

				errorIn.join(100l);
				infoIn.join(100l);
				sb.append("=====================================\n"); //$NON-NLS-1$
			}

			return new ProcessResult(returnValue[0], sb.toString(), null);

		} catch (IOException e) {
			throw new RuntimeException("Failed to perform command: " + e.getLocalizedMessage(), e); //$NON-NLS-1$
		} catch (InterruptedException e) {
			logger.error("Interrupted!"); //$NON-NLS-1$
			sb.append("[FATAL] Interrupted"); //$NON-NLS-1$
			return new ProcessResult(-1, sb.toString(), e);
		}
	}

	public static class ProcessResult {
		public final int returnValue;
		public final String processOutput;
		public final Throwable throwable;

		public ProcessResult(int returnValue, String processOutput, Throwable t) {
			this.returnValue = returnValue;
			this.processOutput = processOutput;
			this.throwable = t;
		}
	}

	static void readStream(StringBuffer sb, String prefix, BufferedReader bufferedReader) {
		String line;
		try {
			while ((line = bufferedReader.readLine()) != null) {
				sb.append(prefix + line + StringHelper.NEW_LINE);
			}
		} catch (IOException e) {
			String msg = "Faild to read from {0} stream: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, prefix, e.getMessage());
			sb.append("[FATAL] "); //$NON-NLS-1$
			sb.append(msg);
			sb.append(StringHelper.NEW_LINE);
		}
	}

	public static void openFile(File pdfPath) {

		ProcessResult processResult;
		if (SystemHelper.isLinux()) {
			processResult = runCommand("xdg-open " + pdfPath.getAbsolutePath()); //$NON-NLS-1$
		} else if (SystemHelper.isMacOS()) {
			processResult = runCommand("open " + pdfPath.getAbsolutePath()); //$NON-NLS-1$
		} else if (SystemHelper.isWindows()) {
			// remove the first char (/) from the report path (/D:/temp.....)
			String pdfFile = pdfPath.getAbsolutePath();
			if (pdfFile.charAt(0) == '/')
				pdfFile = pdfFile.substring(1);
			processResult = runCommand("rundll32 url.dll,FileProtocolHandler " + pdfFile); //$NON-NLS-1$
		} else {
			String msg = MessageFormat.format("Unexpected OS: {0}", SystemHelper.osName); //$NON-NLS-1$
			throw new UnsupportedOperationException(msg);
		}

		logProcessResult(processResult);
	}

	public static void logProcessResult(ProcessResult processResult) {
		if (processResult.returnValue == 0) {
			logger.info("Process executed successfully"); //$NON-NLS-1$
		} else if (processResult.returnValue == -1) {
			logger.error("Process execution failed:\n" + processResult.processOutput); //$NON-NLS-1$
			logger.error(processResult.throwable.getMessage(), processResult.throwable);
		} else {
			String msg = "Process execution was not successful with return value:{0}\n{1}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, processResult.returnValue, processResult.processOutput));
		}
	}
}
