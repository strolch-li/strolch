package li.strolch.privilege.handler;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.helper.LinuxLdapQueryContext;
import li.strolch.privilege.helper.RemoteGroupMappingModel;
import li.strolch.privilege.helper.WindowsLdapQuery;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.policy.PrivilegePolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.AuthenticationException;
import javax.naming.NamingException;
import javax.naming.directory.SearchResult;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;

import static li.strolch.privilege.base.PrivilegeConstants.REALM;
import static li.strolch.privilege.handler.WindowsLdapQueryContext.PLATFORM;
import static li.strolch.privilege.helper.XmlConstants.PARAM_BASE_PATH;
import static li.strolch.privilege.helper.XmlConstants.PARAM_CONFIG_FILE;

public class LdapPrivilegeHandler extends DefaultPrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(LdapPrivilegeHandler.class);

	private WindowsLdapQueryContext queryContext;

	@Override
	public void initialize(ScheduledExecutorService executorService, Map<String, String> parameterMap,
			EncryptionHandler encryptionHandler, PasswordStrengthHandler passwordStrengthHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		super.initialize(executorService, parameterMap, encryptionHandler, passwordStrengthHandler, persistenceHandler,
				userChallengeHandler, ssoHandler, policyMap);

		String realm = parameterMap.get(REALM);
		String basePath = parameterMap.get(PARAM_BASE_PATH);
		String configFileS = parameterMap.get(PARAM_CONFIG_FILE);
		RemoteGroupMappingModel groupMappingModel = new RemoteGroupMappingModel(persistenceHandler);
		groupMappingModel.loadJsonConfig(realm, basePath, configFileS);

		String platform = parameterMap.get(PLATFORM);
		this.queryContext = switch (platform) {
			case "Linux" -> new LinuxLdapQueryContext(parameterMap, groupMappingModel);
			case "Windows" -> new WindowsLdapQueryContext(parameterMap, groupMappingModel);
			default -> throw new IllegalStateException("Unexpected platform: " + platform);
		};
	}

	@Override
	protected User checkCredentialsAndUserState(String username, char[] password) throws AccessDeniedException {

		// first see if this is a local user
		User internalUser = this.persistenceHandler.getUser(username);
		if (internalUser != null && internalUser.getUserState() != UserState.REMOTE)
			return super.checkCredentialsAndUserState(username, password);

		logger.info("User {} tries to login on ldap {}", username, this.queryContext.getProviderUrl());

		// Perform LDAP query
		SearchResult searchResult;
		try (WindowsLdapQuery query = new WindowsLdapQuery(this.queryContext)) {
			searchResult = query.searchLdap(username, password);
		} catch (NamingException e) {
			logger.error("Could not login with user: {} on Ldap", username, e);
			throw new AccessDeniedException("Could not login with user: " + username + " on Ldap", e);
		} finally {
			Arrays.fill(password, '\0');
		}

		// build user
		try {
			User user = this.queryContext.buildUserFromSearchResult(username, searchResult);

			// persist this user
			if (internalUser == null)
				this.persistenceHandler.addUser(user);
			else
				this.persistenceHandler.replaceUser(user);

			if (this.autoPersistOnUserChangesData)
				persistModelAsync();

			return user;

		} catch (AccessDeniedException e) {
			throw e;
		} catch (AuthenticationException e) {
			logger.error("Could not login with user: {} on Ldap", username, e);
			throw new InvalidCredentialsException("Could not login with user: " + username + " on Ldap", e);
		} catch (Exception e) {
			logger.error("Could not login with user: {} on Ldap", username, e);
			throw new AccessDeniedException("Could not login with user: " + username + " on Ldap", e);
		}
	}
}
