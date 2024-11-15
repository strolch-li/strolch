/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.model.internal.User;
import li.strolch.utils.SmtpMailer;
import li.strolch.utils.helper.StringHelper;

import java.util.concurrent.CompletableFuture;

import static li.strolch.privilege.base.PrivilegeConstants.EMAIL;

public class MailUserChallengeHandler extends UserChallengeHandler {

	@Override
	public void sendChallengeToUser(User user, String challenge) {

		String subject = "Mail TAN";

		String text = "Hello "
				+ user.getFirstname()
				+ " "
				+ user.getLastname()
				+ "\n\n"
				+ "You have requested an action which requires you to respond to a challenge.\n\n"
				+ "Please use the following code to response to the challenge:\n\n"
				+ challenge;
		String recipient = user.getEmail();
		if (StringHelper.isEmpty(recipient)) {
			logger.error("User {} has no property {}, so can not initiate challenge!", user.getUsername(), EMAIL);
			return;
		}

		// send e-mail async
		CompletableFuture
				.runAsync(() -> SmtpMailer.getInstance().sendMail(subject, text, recipient))
				.whenComplete((unused, throwable) -> {
					if (throwable == null)
						logger.error("Sent Mail TAN e-mail for user {}", user, throwable);
					else
						logger.error("Failed to send e-mail for user {}", user, throwable);
				});
	}
}
