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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.mail.BodyPart;
import jakarta.mail.Multipart;
import jakarta.mail.Session;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeMessage;
import org.bouncycastle.openpgp.api.OpenPGPCertificate;
import org.bouncycastle.openpgp.api.OpenPGPKey;
import org.junit.Test;
import org.pgpainless.PGPainless;
import org.pgpainless.decryption_verification.ConsumerOptions;
import org.pgpainless.decryption_verification.DecryptionStream;
import org.pgpainless.key.protection.SecretKeyRingProtector;
import org.pgpainless.util.Passphrase;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.util.Properties;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.*;

public class ResourceMonitorEncryptionTest {

	@Test
	public void shouldEncryptResourceMonitorJsonAttachment() throws Exception {
		String report = """
				{"schemaVersion":1,"monitorId":"private-monitor-7531","jobId":"monitor-job","realm":"test",
				 "sampledAt":"2026-01-01T00:00:00Z","notificationMode":"THRESHOLD","events":[],
				 "metrics":[{"key":"disk:/private-volume-7531","unit":"percent","value":93.751,
				 "threshold":90.0,"status":"BREACHED","totalBytes":9876543210,"freeBytes":617283945}]}
				""";
		OpenPGPKey key;
		try (FileInputStream in = new FileInputStream(SmtpMailerTest.SIGNING_KEY_FILE_NAME)) {
			key = PGPainless.getInstance().readKey().parseKey(in);
		}
		assertNotNull(key);

		CapturingMailer mailer = new CapturingMailer(key.toCertificate());
		mailer.setSigningKeyFileName(SmtpMailerTest.SIGNING_KEY_FILE_NAME, SmtpMailerTest.SIGNING_KEY_PASSWORD);
		mailer.addRecipientPublicKeyFileName(SmtpMailerTest.SIGNING_KEY_FILE_NAME);
		mailer.sendEncryptedEmailWithAttachment("recipient@example.com", "Resource monitor report",
				"This is an encrypted mail. Please decrypt the attached file for details.",
				"A resource monitor report is attached.", "Resource_monitor_report.txt",
				new MailAttachment(report, "resource-monitor.json", "application/json", true, true));

		assertNotNull(mailer.message);
		assertEquals("recipient@example.com", mailer.recipients[0].getAddress());
		mailer.message.saveChanges();
		ByteArrayOutputStream serialized = new ByteArrayOutputStream();
		mailer.message.writeTo(serialized);
		MimeMessage message = new MimeMessage(Session.getInstance(new Properties()),
				new ByteArrayInputStream(serialized.toByteArray()));
		assertEquals("Resource monitor report", message.getSubject());
		assertTrue(message.isMimeType("multipart/mixed"));
		Multipart multipart = (Multipart) message.getContent();
		assertEquals(3, multipart.getCount());
		BodyPart wrapper = multipart.getBodyPart(0);
		assertTrue(wrapper.isMimeType("text/plain"));
		String wrapperText = (String) wrapper.getContent();
		assertEquals("This is an encrypted mail. Please decrypt the attached file for details.", wrapperText);

		for (String sensitive : new String[] {"private-monitor-7531", "private-volume-7531", "93.751", "9876543210", "617283945"}) {
			assertFalse(message.getSubject().contains(sensitive));
			assertFalse(wrapperText.contains(sensitive));
			assertFalse(serialized.toString(UTF_8).contains(sensitive));
		}

		BodyPart attachment = null;
		for (int i = 0; i < multipart.getCount(); i++) {
			BodyPart part = multipart.getBodyPart(i);
			if ("resource-monitor.json.asc".equals(part.getFileName())) {
				assertNull("Duplicate resource monitor attachment", attachment);
				attachment = part;
			}
		}
		assertNotNull("Missing resource monitor attachment", attachment);
		assertTrue(attachment.isMimeType("application/pgp-encrypted"));
		ConsumerOptions options = ConsumerOptions.get(PGPainless.getInstance())
				.addDecryptionKey(key, SecretKeyRingProtector.unlockAnyKeyWith(new Passphrase(SmtpMailerTest.SIGNING_KEY_PASSWORD)));
		String decrypted;
		try (DecryptionStream in = PGPainless.getInstance().processMessage().onInputStream(attachment.getInputStream())
				.withOptions(options)) {
			decrypted = new String(in.readAllBytes(), UTF_8);
		}
		JsonObject actual = JsonParser.parseString(decrypted).getAsJsonObject();
		assertEquals(JsonParser.parseString(report), actual);
		JsonObject metric = actual.getAsJsonArray("metrics").get(0).getAsJsonObject();
		assertEquals("disk:/private-volume-7531", metric.get("key").getAsString());
		assertEquals(93.751, metric.get("value").getAsDouble(), 0.000001);
		assertEquals(9876543210L, metric.get("totalBytes").getAsLong());
	}

	private static class CapturingMailer extends SmtpMailer {

		private final OpenPGPCertificate certificate;
		private InternetAddress[] recipients;
		private MimeMessage message;

		private CapturingMailer(OpenPGPCertificate certificate) {
			super("sender@example.com", "localhost", 25, false, false, null, null);
			this.certificate = certificate;
		}

		@Override
		protected OpenPGPCertificate getRecipientCertificate(String recipientPublicKeyFileName) {
			return this.certificate;
		}

		@Override
		protected void send(InternetAddress[] recipients, MimeMessage message) {
			this.recipients = recipients;
			this.message = message;
		}
	}
}