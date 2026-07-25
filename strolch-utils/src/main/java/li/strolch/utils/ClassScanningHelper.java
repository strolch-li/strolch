package li.strolch.utils;

import java.io.File;

public class ClassScanningHelper {

	public static boolean shouldIgnorePropertyFile(String name) {
		return name.startsWith("META-INF")
				|| name.equals("ENV.properties")
				|| name.equals("agentVersion.properties")
				|| name.equals("appVersion.properties")
				|| name.equals("componentVersion.properties")
				|| name.contains("_db_version");
	}

	public static boolean shouldIgnoreClassFile(String name) {
		return name.contains("META-INF") || name.startsWith("module-info");
	}

	public static boolean shouldIgnoreFile(File file) {
		return shouldIgnoreFile(file.getName());
	}

	public static boolean shouldIgnoreFile(String name) {
		return name.contains("aopalliance")
				|| name.contains("activation")
				|| name.contains("antlr")
				|| name.contains("assertj-core")
				|| name.startsWith("com.sun")
				|| name.startsWith("commonj.")
				|| name.startsWith("commons-")
				|| name.startsWith("jackson-")
				|| name.startsWith("junit-")
				|| name.startsWith("idea_rt")
				|| name.startsWith("junit")
				|| name.startsWith("hapi-")
				|| name.startsWith("jaxb-")
				|| name.startsWith("angus-")
				|| name.startsWith("org.hl7.")
				|| name.startsWith("org.glassfish.")
				|| name.startsWith("listenablefuture-")
				|| name.startsWith("j2objc-annotations")
				|| name.startsWith("failureaccess-")
				|| name.startsWith("error_prone_")
				|| name.startsWith("guava-")
				|| name.startsWith("org.eclipse")
				|| name.startsWith("javax")
				|| name.startsWith("pgpainless")
				|| name.startsWith("kotlin")
				|| name.startsWith("annotations-")
				|| name.startsWith("storage-")
				|| name.startsWith("afs-")
				|| name.startsWith("base-")
				|| name.startsWith("persistence-")
				|| name.startsWith("classgraph-")
				|| name.startsWith("snakeyaml-")
				|| name.startsWith("opentest4j-")
				|| name.startsWith("apiguardian-")
				|| name.startsWith("hamcrest-")
				|| name.startsWith("jaxws")
				|| name.startsWith("jaxrs")
				|| name.startsWith("jaxb")
				// bouncy castle
				|| name.contains("-jdk18on-")
				|| name.contains("jsr305")
				|| name.contains("c3p0")
				|| name.contains("camel")
				|| name.contains("checker-qual")
				|| name.contains("cron")
				|| name.contains("FastInfoset")
				|| name.contains("gmbal")
				|| name.contains("grizzly")
				|| name.contains("gson")
				|| name.contains("ha-api")
				|| name.contains("HikariCP")
				|| name.contains("hk2")
				|| name.contains("icu4j")
				|| name.contains("jakarta")
				|| name.contains("javassist")
				|| name.contains("jersey")
				|| name.contains("joda-time")
				|| name.contains("logback")
				|| name.contains("management-api")
				|| name.contains("mchange-commons-java")
				|| name.contains("mimepull")
				|| name.contains("org.abego.treelayout")
				|| name.contains("osgi")
				|| name.contains("pfl-basic")
				|| name.contains("pfl-tf")
				|| name.contains("policy-2.7.10")
				|| name.contains("postgresql")
				|| name.contains("quartz")
				|| name.contains("saaj-impl")
				|| name.contains("sax")
				|| name.contains("slf4j")
				|| name.contains("ST4")
				|| name.contains("stax-ex")
				|| name.contains("stax2-api")
				|| name.contains("streambuffer")
				|| name.contains("tyrus")
				|| name.contains("validation-api")
				|| name.contains("yasson");
	}

}
