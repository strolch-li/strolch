/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent.impl;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelStatistics;
import li.strolch.model.xml.XmlModelSaxFileReader;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.PrivilegeContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

import static li.strolch.utils.helper.StringHelper.formatNanoDuration;

public class XmlModelLoader {

	private static final Logger logger = LoggerFactory.getLogger(XmlModelLoader.class);

	private final String realm;
	private final boolean verbose;
	private final File modelFile;

	public XmlModelLoader(String realm, boolean verbose, File modelFile) {
		this.realm = realm;
		this.verbose = verbose;
		this.modelFile = modelFile;
	}

	public void load(PrivilegeContext privilegeContext, StrolchRealm realm) {

		ModelStatistics statistics;
		try (StrolchTransaction tx = realm.openTx(privilegeContext.getCertificate(), "strolch_boot", false)) {
			InMemoryElementListener elementListener = new InMemoryElementListener(tx);

			// explicitly deny updating, so that we can detect XML files with duplicates
			elementListener.setUpdateResources(false);
			elementListener.setUpdateOrders(false);
			elementListener.setUpdateActivities(false);
			elementListener.setFailOnUpdate(true);

			XmlModelSaxFileReader handler = new XmlModelSaxFileReader(elementListener, this.modelFile, true,
					this.verbose);
			handler.parseFile();
			statistics = handler.getStatistics();
			tx.commitOnClose();
		}

		String durationS = formatNanoDuration(statistics.durationNanos);
		logger.info("Loaded XML Model file {} for realm {} took {}.", this.modelFile.getName(), realm.getRealm(),
				durationS);
		logger.info("Loaded {} Orders, {} Resource, {} Activities", statistics.nrOfOrders, statistics.nrOfResources,
				statistics.nrOfActivities);
	}
}
