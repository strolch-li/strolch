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
