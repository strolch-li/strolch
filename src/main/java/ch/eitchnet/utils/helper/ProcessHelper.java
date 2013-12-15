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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ProcessHelper {

	private static final Logger logger = LoggerFactory.getLogger(ProcessHelper.class);

	public static ProcessResult runCommand(String command) {
		final StringBuffer sb = new StringBuffer();
		sb.append("=====================================\n");
		try {

			final Process process = Runtime.getRuntime().exec(command);

			final BufferedReader errorStream = new BufferedReader(new InputStreamReader(process.getErrorStream()));
			Thread errorIn = new Thread("errorIn") {
				@Override
				public void run() {
					ProcessHelper.readStream(sb, "[ERROR] ", errorStream);
				}
			};
			errorIn.start();

			final BufferedReader inputStream = new BufferedReader(new InputStreamReader(process.getInputStream()));
			Thread infoIn = new Thread("infoIn") {
				@Override
				public void run() {
					ProcessHelper.readStream(sb, "[INFO] ", inputStream);
				}
			};
			infoIn.start();

			int returnValue = process.waitFor();

			errorIn.join(100l);
			infoIn.join(100l);
			sb.append("=====================================\n");

			return new ProcessResult(returnValue, sb.toString(), null);

		} catch (IOException e) {
			throw new RuntimeException("Failed to perform command: " + e.getLocalizedMessage(), e);
		} catch (InterruptedException e) {
			ProcessHelper.logger.error("Interrupted!");
			sb.append("[FATAL] Interrupted");
			return new ProcessResult(-1, sb.toString(), e);
		}
	}

	public static ProcessResult runCommand(File workingDirectory, String... commandAndArgs) {

		if (!workingDirectory.exists())
			throw new RuntimeException("Working directory does not exist at " + workingDirectory.getAbsolutePath());
		if (commandAndArgs == null || commandAndArgs.length == 0)
			throw new RuntimeException("No command passed!");

		final StringBuffer sb = new StringBuffer();
		sb.append("=====================================\n");
		try {

			ProcessBuilder processBuilder = new ProcessBuilder(commandAndArgs);
			processBuilder.environment();
			processBuilder.directory(workingDirectory);

			final Process process = processBuilder.start();

			final BufferedReader errorStream = new BufferedReader(new InputStreamReader(process.getErrorStream()));
			Thread errorIn = new Thread("errorIn") {
				@Override
				public void run() {
					ProcessHelper.readStream(sb, "[ERROR] ", errorStream);
				}
			};
			errorIn.start();

			final BufferedReader inputStream = new BufferedReader(new InputStreamReader(process.getInputStream()));
			Thread infoIn = new Thread("infoIn") {
				@Override
				public void run() {
					ProcessHelper.readStream(sb, "[INFO] ", inputStream);
				}
			};
			infoIn.start();

			int returnValue = process.waitFor();

			errorIn.join(100l);
			infoIn.join(100l);
			sb.append("=====================================\n");

			return new ProcessResult(returnValue, sb.toString(), null);

		} catch (IOException e) {
			throw new RuntimeException("Failed to perform command: " + e.getLocalizedMessage(), e);
		} catch (InterruptedException e) {
			ProcessHelper.logger.error("Interrupted!");
			sb.append("[FATAL] Interrupted");
			return new ProcessResult(-1, sb.toString(), e);
		}
	}

	public static class ProcessResult {
		public final int returnValue;
		public final String processOutput;
		public final Throwable t;

		public ProcessResult(int returnValue, String processOutput, Throwable t) {
			this.returnValue = returnValue;
			this.processOutput = processOutput;
			this.t = t;
		}
	}

	private static void readStream(StringBuffer sb, String prefix, BufferedReader bufferedReader) {
		String line;
		try {
			while ((line = bufferedReader.readLine()) != null) {
				sb.append(prefix + line + "\n");
			}
		} catch (IOException e) {
			String msg = "Faild to read from " + prefix + " stream: " + e.getLocalizedMessage();
			sb.append("[FATAL] " + msg + "\n");
		}
	}

	public static void openFile(File pdfPath) {

		ProcessResult processResult;
		if (SystemHelper.isLinux()) {
			processResult = ProcessHelper.runCommand("xdg-open " + pdfPath.getAbsolutePath());
		} else if (SystemHelper.isMacOS()) {
			processResult = ProcessHelper.runCommand("open " + pdfPath.getAbsolutePath());
		} else if (SystemHelper.isWindows()) {
			// remove the first char (/) from the report path (/D:/temp.....)
			String pdfFile = pdfPath.getAbsolutePath();
			if (pdfFile.charAt(0) == '/')
				pdfFile = pdfFile.substring(1);
			processResult = ProcessHelper.runCommand("rundll32 url.dll,FileProtocolHandler " + pdfFile);
		} else {
			throw new UnsupportedOperationException("Unexpected OS: " + SystemHelper.osName);
		}

		ProcessHelper.logProcessResult(processResult);
	}

	public static void logProcessResult(ProcessResult processResult) {
		if (processResult.returnValue == 0) {
			ProcessHelper.logger.info("Process executed successfully");
		} else if (processResult.returnValue == -1) {
			ProcessHelper.logger.error("Process execution failed:\n" + processResult.processOutput);
			ProcessHelper.logger.error(processResult.t.getMessage(), processResult.t);
		} else {
			ProcessHelper.logger.info("Process execution was not successful with return value:"
					+ processResult.returnValue + "\n" + processResult.processOutput);
		}
	}
}
