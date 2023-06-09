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
package li.strolch.rest;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import li.strolch.rest.endpoint.*;
import li.strolch.rest.filters.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchRestfulClasses {

	public static Set<Class<?>> restfulClasses;
	public static Set<Class<?>> providerClasses;

	static {

		Set<Class<?>> restfulClasses = new HashSet<>();

		restfulClasses.add(AuthenticationService.class);
		restfulClasses.add(StrolchJobsResource.class);
		restfulClasses.add(ReportResource.class);
		restfulClasses.add(ControlResource.class);
		restfulClasses.add(Inspector.class);
		restfulClasses.add(I18nService.class);
		restfulClasses.add(VersionQuery.class);
		restfulClasses.add(ModelQuery.class);
		restfulClasses.add(EnumQuery.class);
		restfulClasses.add(Languages.class);
		restfulClasses.add(OperationsLogResource.class);

		// privilege
		restfulClasses.add(PrivilegeUsersService.class);
		restfulClasses.add(PrivilegeRolesService.class);
		restfulClasses.add(PrivilegePoliciesService.class);
		restfulClasses.add(UserSessionsService.class);
		restfulClasses.add(AuditsService.class);

		Set<Class<?>> providerClasses = new HashSet<>();
		providerClasses.add(StrolchRestfulExceptionMapper.class);
		providerClasses.add(AccessControlResponseFilter.class);
		providerClasses.add(AuthenticationRequestFilter.class);
		providerClasses.add(AuthenticationResponseFilter.class);
		providerClasses.add(CharsetResponseFilter.class);
		providerClasses.add(HttpCacheResponseFilter.class);

		StrolchRestfulClasses.restfulClasses = Collections.unmodifiableSet(restfulClasses);
		StrolchRestfulClasses.providerClasses = Collections.unmodifiableSet(providerClasses);
	}

	public static Set<Class<?>> getRestfulClasses() {
		return restfulClasses;
	}

	public static Set<Class<?>> getProviderClasses() {
		return providerClasses;
	}
}
