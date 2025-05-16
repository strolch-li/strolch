/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils;

import jakarta.mail.BodyPart;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeBodyPart;
import jakarta.mail.internet.MimeMessage;
import jakarta.mail.internet.MimeMultipart;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Properties;

public class SimulatedSmtpMailer extends SmtpMailer {

	public static volatile boolean debug;
	private static final Logger logger = LoggerFactory.getLogger(SimulatedSmtpMailer.class);
	private static SimulatedSmtpMailer instance;

	/**
	 * <p>
	 * Initializes the SMTP Mailer with the given properties.
	 * </p>
	 *
	 * @param properties the properties to be used to initialize the mailer
	 */
	public static SimulatedSmtpMailer init(Properties properties) {
		instance = new SimulatedSmtpMailer(properties);
		return instance;
	}

	/**
	 * <p>
	 * Initializes the SMTP Mailer with the given properties.
	 * </p>
	 *
	 * @param fromAddress the from address
	 * @param host        the host to send the mails to
	 * @param port        the host port
	 * @param auth        boolean to indicate the connection to the host requires auth
	 * @param startTls    should STARTTLS be send to the host
	 * @param username    the username for connection authorization
	 * @param password    the password for connection authorization
	 *
	 * @return
	 */
	public static SimulatedSmtpMailer init(String fromAddress, String host, int port, boolean auth, boolean startTls,
			String username, String password) {
		instance = new SimulatedSmtpMailer(fromAddress, host, port, auth, startTls, username, password);
		return instance;
	}

	/**
	 * Returns the instance
	 *
	 * @return the instance
	 */
	public static SimulatedSmtpMailer getInstance() {
		if (instance == null)
			throw new RuntimeException("Instance not yet configured! Call init first!");
		return instance;
	}

	public SimulatedSmtpMailer(Properties properties) {
		super(properties);
	}

	public SimulatedSmtpMailer(String fromAddress, String host, int port, boolean auth, boolean startTls,
			String username, String password) {
		super(fromAddress, host, port, auth, startTls, username, password);
	}

	@Override
	protected void send(InternetAddress[] recipients, MimeMessage message) {
		try {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			message.writeTo(out);

			Object content = message.getContent();
			String messageAsString = switch (content) {
				case String stringContent -> stringContent;
				case MimeMultipart mimeMultipart -> {
					StringBuilder sb = new StringBuilder();
					for (int i = 0; i < mimeMultipart.getCount(); i++) {
						BodyPart bodyPart = mimeMultipart.getBodyPart(i);
						addBodyPart(bodyPart.getFileName(), sb, bodyPart.getFileName(), bodyPart.getContentType(),
								bodyPart.getContent());
					}
					yield sb.toString();
				}
				case MimeBodyPart bodyPart -> {
					StringBuilder sb = new StringBuilder();
					addBodyPart(bodyPart.getFileName(), sb, bodyPart.getFileName(), bodyPart.getContentType(),
							bodyPart.getContent());
					yield sb.toString();
				}
				case null, default -> out.toString();
			};

			if (!debug) {
				messageAsString = messageAsString.substring(0, 300);
			}
			logger.info("""
					Simulated sending of the following message:
					To: {}
					Subject: {}
					Body:
					{}""", addressesToString(recipients), message.getSubject(), messageAsString);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to print message!", e);
		}
	}

	private static void addBodyPart(String bodyPart, StringBuilder sb, String bodyPart1, String bodyPart2,
			Object bodyPart3) throws MessagingException, IOException {
		if (bodyPart != null)
			sb.append("File: ").append(bodyPart1).append(":\n");
		sb.append("Content-Type: ").append(bodyPart2).append("\n");
		sb.append("Content: ").append(bodyPart3.toString());
		sb.append("\n");
	}
}
