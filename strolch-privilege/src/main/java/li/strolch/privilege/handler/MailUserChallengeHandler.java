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

package li.strolch.privilege.handler;

import jakarta.mail.internet.AddressException;
import jakarta.mail.internet.InternetAddress;
import li.strolch.privilege.model.internal.User;
import li.strolch.utils.SmtpMailer;

import java.util.concurrent.CompletableFuture;

import static li.strolch.privilege.base.PrivilegeConstants.EMAIL;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

public class MailUserChallengeHandler extends UserChallengeHandler {

	@Override
	public void sendChallengeToUser(User user, String challenge) {

		String subject = "Mail TAN";
		String text = """
				Hello %s %s
				
				You have requested an action which requires you to respond to a challenge.
				
				Please use the following code to response to the challenge:
				
				    %s
				
				""".formatted(user.getFirstname(), user.getLastname(), challenge);
		String recipient = trimOrEmpty(user.getEmail());
		if (isEmpty(recipient)) {
			logger.error("User {} has no or empty property {}, so can not initiate challenge!", user.getUsername(),
					EMAIL);
			return;
		}

		// validate email address
		try {
			InternetAddress.parse(recipient);
		} catch (AddressException e) {
			logger.error("Failed to parse address: {}", recipient, e);
			throw new IllegalArgumentException("Email address " + recipient + " is invalid!");
		}

		// send e-mail async
		CompletableFuture
				.runAsync(() -> SmtpMailer.getInstance().sendMailSignedIfAvailable(recipient, subject, text))
				.whenComplete((_, throwable) -> {
					if (throwable == null)
						logger.info("Sent Mail TAN email for user {} to {}", user, recipient);
					else
						logger.error("Failed to send email for user {} to {}", user, recipient, throwable);
				});
	}
}
