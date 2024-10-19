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
package li.strolch.utils.helper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ProcessHelper {

	private static final Logger logger = LoggerFactory.getLogger(ProcessHelper.class);

	public static ProcessResult runCommand(String... commandAndArgs) {
		return runCommand(null, commandAndArgs);
	}

	public static ProcessResult runCommand(File workingDirectory, String... commandAndArgs) {
		return runCommand(30, TimeUnit.SECONDS, workingDirectory, commandAndArgs);
	}

	public static ProcessResult runCommand(long timeout, TimeUnit unit, String... commandAndArgs) {
		return runCommand(timeout, unit, null, commandAndArgs);
	}

	public static ProcessResult runCommand(long timeout, TimeUnit unit, File workingDirectory,
			String... commandAndArgs) {

		if (workingDirectory != null && !workingDirectory.isDirectory()) {
			String msg = "Working directory does not exist or is not a directory at {0}";
			msg = MessageFormat.format(msg, workingDirectory.getAbsolutePath());
			throw new RuntimeException(msg);
		}
		if (commandAndArgs == null || commandAndArgs.length == 0)
			throw new RuntimeException("No command passed!");

		final StringBuffer sb = new StringBuffer();
		sb.append("=====================================\n");
		try {

			ProcessBuilder pb = new ProcessBuilder(commandAndArgs);
			pb.environment();
			pb.directory(workingDirectory);

			long start = System.nanoTime();
			logger.info("Starting command (Timeout {} {}) {}", timeout, unit.name(), String.join(" ", commandAndArgs));
			final Process process = pb.start();
			int[] returnValue = new int[1];

			try (BufferedReader errorStream = new BufferedReader(new InputStreamReader(process.getErrorStream()));
				 BufferedReader inputStream = new BufferedReader(new InputStreamReader(process.getInputStream()))) {

				Thread errorIn = new Thread(() -> readStream(sb, "[ERROR] ", errorStream), "errorIn");
				errorIn.start();

				Thread infoIn = new Thread(() -> readStream(sb, "[INFO] ", inputStream), "infoIn");
				infoIn.start();

				boolean ok = process.waitFor(timeout, unit);
				if (!ok)
					logger.error("Command failed to end before timeout or failed to execute.");

				if (!process.isAlive()) {
					returnValue[0] = process.exitValue();
				} else {
					logger.error("Forcibly destroying as still running...");
					process.destroyForcibly();
					process.waitFor(5, TimeUnit.SECONDS);
					returnValue[0] = -1;
				}

				errorIn.join(100L);
				infoIn.join(100L);
				sb.append("=====================================\n");
			}

			logger.info("Command ended after {}", StringHelper.formatNanoDuration(System.nanoTime() - start));
			return new ProcessResult(returnValue[0], sb.toString(), null);

		} catch (IOException e) {
			throw new RuntimeException("Failed to perform command: " + e.getLocalizedMessage(), e);
		} catch (InterruptedException e) {
			logger.error("Interrupted!");
			sb.append("[FATAL] Interrupted");
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
				sb.append(prefix).append(line).append(StringHelper.NEW_LINE);
			}
		} catch (IOException e) {
			String msg = "Faild to read from {0} stream: {1}";
			msg = MessageFormat.format(msg, prefix, e.getMessage());
			sb.append("[FATAL] ");
			sb.append(msg);
			sb.append(StringHelper.NEW_LINE);
		}
	}

	public static void openFile(File pdfPath) {

		ProcessResult processResult;
		if (SystemHelper.isLinux()) {
			processResult = runCommand("xdg-open " + pdfPath.getAbsolutePath());
		} else if (SystemHelper.isMacOS()) {
			processResult = runCommand("open " + pdfPath.getAbsolutePath());
		} else if (SystemHelper.isWindows()) {
			// remove the first char (/) from the report path (/D:/temp.....)
			String pdfFile = pdfPath.getAbsolutePath();
			if (pdfFile.charAt(0) == '/')
				pdfFile = pdfFile.substring(1);
			processResult = runCommand("rundll32 url.dll,FileProtocolHandler " + pdfFile);
		} else {
			String msg = MessageFormat.format("Unexpected OS: {0}", SystemHelper.osName);
			throw new UnsupportedOperationException(msg);
		}

		logProcessResult(processResult);
	}

	public static void logProcessResult(ProcessResult processResult) {
		if (processResult.returnValue == 0) {
			logger.info("Process executed successfully");
		} else if (processResult.returnValue == -1) {
			logger.error("Process execution failed:\n{}", processResult.processOutput);
			logger.error(processResult.throwable.getMessage(), processResult.throwable);
		} else {
			logger.info("Process execution was not successful with return value:{}\n{}", processResult.returnValue,
					processResult.processOutput);
		}
	}
}
