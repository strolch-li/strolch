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
package li.strolch.runtime.query.enums;

import java.util.Locale;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface EnumHandler {

	StrolchEnum getEnum(Certificate certificate, String name, Locale locale);

	StrolchEnum getEnum(Certificate certificate, String name, Locale locale, boolean withoutHidden);

	StrolchEnum getEnum(StrolchTransaction tx, String name);

	StrolchEnum getEnum(StrolchTransaction tx, String name, boolean withoutHidden);
}
