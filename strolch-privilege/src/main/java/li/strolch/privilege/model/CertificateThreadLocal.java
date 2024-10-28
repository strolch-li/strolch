/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.model;

public class CertificateThreadLocal extends ThreadLocal<Certificate> {

	private static final CertificateThreadLocal instance = new CertificateThreadLocal();

	public static boolean hasCert() {
		return instance.get() != null;
	}

	public static Certificate getCert() {
		Certificate cert = instance.get();
		if (cert == null)
			throw new IllegalStateException("No Cert available on thread " + Thread.currentThread().getName());
		return cert;
	}

	public static void setCert(Certificate cert) {
		if (instance.get() != null)
			throw new IllegalStateException("THIS THREAD HAS ALREADY HAS A CERT!");
		instance.set(cert);
	}

	public static void removeCert() {
		instance.remove();
	}
}
