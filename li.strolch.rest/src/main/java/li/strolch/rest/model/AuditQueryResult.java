/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.rest.model;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.model.audit.Audit;
import li.strolch.utils.helper.StringHelper;

@XmlRootElement(name = "AuditQueryResult")
@XmlAccessorType(XmlAccessType.NONE)
public class AuditQueryResult {

	@XmlAttribute(name = "msg")
	private String msg;

	@XmlElement(name = "audits")
	private List<Audit> audits;

	public AuditQueryResult() {
		this.msg = StringHelper.DASH;
	}

	public AuditQueryResult(List<Audit> audits) {
		this.msg = StringHelper.DASH;
		this.audits = audits;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}

	public List<Audit> getAudits() {
		return audits;
	}

	public void setAudits(List<Audit> audits) {
		this.audits = audits;
	}
}
