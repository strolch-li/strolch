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

package li.strolch.utils;

public record MailAttachment(String attachment, String fileName, String type, boolean sign, boolean encrypt) {

	public MailAttachment {
		if (attachment == null || attachment.isBlank())
			throw new IllegalArgumentException("Attachment cannot be null or empty");
		if (fileName == null || fileName.isBlank())
			throw new IllegalArgumentException("File name cannot be null or empty");
		if (fileName.matches(".*[<>:\"/\\\\|?*].*"))
			throw new IllegalArgumentException("File name contains invalid characters");
		if (type == null || !type.matches("[a-zA-Z0-9-]+/[a-zA-Z0-9-+.]+"))
			throw new IllegalArgumentException("Invalid MIME type format");
	}

	public MailAttachment(String attachment, String fileName) {
		this(attachment, fileName, "text/plain");
	}

	public MailAttachment(String attachment, String fileName, String type) {
		this(attachment, fileName, type, false, false);
	}

	public MailAttachment(String attachment, String fileName, boolean sign) {
		this(attachment, fileName, "text/plain", sign, false);
	}

	public MailAttachment(String attachment, String fileName, String type, boolean sign) {
		this(attachment, fileName, type, sign, false);
	}
}
