package li.strolch.privilege.handler;

import static li.strolch.privilege.base.PrivilegeConstants.EMAIL;

import java.util.concurrent.CompletableFuture;

import li.strolch.privilege.model.internal.User;
import li.strolch.utils.SmtpMailer;
import li.strolch.utils.helper.StringHelper;

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
