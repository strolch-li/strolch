package li.strolch.utils;

import static org.junit.Assert.*;

import li.strolch.utils.helper.StringHelper;
import org.junit.Test;

/**
 * Tests the {@link Version} class
 */
public class VersionTest {

	@Test
	public void shouldParseMajoMinoMicro() {
		Version version = Version.valueOf("1.0.2");
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(2, version.getMicro());
		assertEquals(StringHelper.EMPTY, version.getQualifier());
		assertEquals(StringHelper.EMPTY, version.getQualifier());

		assertEquals("1.0.2", version.toString());
	}

	@Test
	public void shouldParseVersion() {
		{
			Version version = Version.valueOf("7.5.6.1");
			assertEquals(7, version.getMajor());
			assertEquals(5, version.getMinor());
			assertEquals(6, version.getMicro());
			assertEquals("1", version.getQualifier());
			assertTrue(version.isOsgiStyle());
		}
		{
			Version version = Version.valueOf("7.5.6-1");
			assertEquals(7, version.getMajor());
			assertEquals(5, version.getMinor());
			assertEquals(6, version.getMicro());
			assertEquals("1", version.getQualifier());
			assertFalse(version.isOsgiStyle());
		}
	}

	@Test
	public void shouldCompareVersions() {
		assertEquals(0, Version.valueOf("7.5.6-1").compareTo(Version.valueOf("7.5.6-1")));
		assertTrue(Version.valueOf("7.5.6-1").compareTo(Version.valueOf("7.5.6-2")) < 0);
		assertTrue(Version.valueOf("7.5.6-1").compareTo(Version.valueOf("7.6.0")) < 0);
		assertTrue(Version.valueOf("7.5.6-1").compareTo(Version.valueOf("7.6.1")) < 0);
		assertTrue(Version.valueOf("7.5.6-alpha").compareTo(Version.valueOf("7.6.1-beta")) < 0);
		assertTrue(Version.valueOf("7.7.0-0").compareTo(Version.valueOf("7.6.99-9")) > 0);
		assertTrue(Version.valueOf("0.0.1.a").compareTo(Version.valueOf("0.0.1.b")) < 0);
		assertTrue(Version.valueOf("0.0.1.b").compareTo(Version.valueOf("0.0.1.a")) > 0);
		assertTrue(Version.valueOf("0.0.1.a").compareTo(Version.valueOf("0.0.1.c")) < 0);
		assertTrue(Version.valueOf("0.0.1.a").compareTo(Version.valueOf("0.0.1.aa")) < 0);
	}

	@Test
	public void shouldConvertToMajorMinorString() {
		assertEquals("7.6", Version.valueOf("7.6.1-0").toMajorAndMinorString());
	}

	@Test
	public void shouldKnowAboutBeingFullyQualified() {
		assertFalse(Version.valueOf("7").isFullyQualified());
		assertFalse(Version.valueOf("7.6").isFullyQualified());
		assertFalse(Version.valueOf("7.6.1").isFullyQualified());
		assertTrue(Version.valueOf("7.6.1-0").isFullyQualified());
	}

	@Test
	public void shouldDealWithEclipseStyleSnapshotQualifier() {
		assertEquals(Version.valueOf("7.6.1-SNAPSHOT").toOsgiStyleString(), "7.6.1.qualifier");
		assertEquals(Version.valueOf("7.6.1-SNAPSHOT").toMavenStyleString(), "7.6.1-SNAPSHOT");
		assertEquals(Version.valueOf("7.6.1.qualifier").toOsgiStyleString(), "7.6.1.qualifier");
		assertEquals(Version.valueOf("7.6.1.qualifier").toMavenStyleString(), "7.6.1-SNAPSHOT");
	}

	@Test
	public void shouldIncreaseVersion() {

		Version increased = Version.emptyVersion.add(0, 0, 0);
		assertEquals("0.0.0", increased.toString());

		increased = increased.add(0, 0, 1);
		assertEquals("0.0.1", increased.toString());

		increased = increased.add(0, 1, 0);
		assertEquals("0.1.1", increased.toString());

		increased = increased.add(1, 0, 0);
		assertEquals("1.1.1", increased.toString());

		increased = increased.add(-1, 0, 0);
		assertEquals("0.1.1", increased.toString());

		increased = increased.add(0, -1, 0);
		assertEquals("0.0.1", increased.toString());

		increased = increased.add(0, 0, -1);
		assertEquals("0.0.0", increased.toString());
	}
}
