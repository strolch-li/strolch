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
package li.strolch.persistence.impl;

import java.util.List;

import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

public class XmlResourceDao extends AbstractDao<Resource> implements ResourceDao {

	protected XmlResourceDao(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassType() {
		return Tags.RESOURCE;
	}

	@Override
	public List<Resource> doQuery(ResourceQuery query) {
		// TODO Auto-generated method stub
		return null;
	}
}
