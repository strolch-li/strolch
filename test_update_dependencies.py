#!/usr/bin/env python3
import os
import sys
import shutil
import tempfile
import unittest
from unittest.mock import patch, MagicMock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import update_dependencies
from update_dependencies import (
    MavenVersion,
    PreferencesManager,
    is_prerelease,
    update_property_in_pom,
    update_direct_dependency_in_pom,
    discover_targets,
    run_updates
)


class TestMavenVersion(unittest.TestCase):

    def test_prerelease_detection(self):
        self.assertFalse(is_prerelease("2.0.17"))
        self.assertFalse(is_prerelease("1.5.32"))
        self.assertFalse(is_prerelease("4.9.3.0"))
        self.assertFalse(is_prerelease("1.2.3.Final"))
        self.assertFalse(is_prerelease("1.2.3-RELEASE"))

        self.assertTrue(is_prerelease("2.1.0-alpha1"))
        self.assertTrue(is_prerelease("3.0.0-M1"))
        self.assertTrue(is_prerelease("2.0.0-RC1"))
        self.assertTrue(is_prerelease("1.0.0-SNAPSHOT"))
        self.assertTrue(is_prerelease("2.1.0-ea"))
        self.assertTrue(is_prerelease("1.0.0-beta-2"))

    def test_version_parts_and_ordering(self):
        v1 = MavenVersion("2.0.17")
        v2 = MavenVersion("2.0.20")
        v3 = MavenVersion("2.1.0-alpha1")
        v4 = MavenVersion("2.1.0")
        v5 = MavenVersion("3.0.0")

        self.assertEqual(v1.major, 2)
        self.assertEqual(v1.minor, 0)
        self.assertEqual(v1.patch, 17)

        self.assertTrue(v1 < v2)
        self.assertTrue(v2 < v4)
        self.assertTrue(v3 < v4)
        self.assertTrue(v4 < v5)

        self.assertTrue(v1.is_same_major_minor(v2))
        self.assertFalse(v1.is_same_major_minor(v4))
        self.assertFalse(v1.is_same_major_minor(v5))

    def test_upgrade_type(self):
        curr = MavenVersion("2.0.17")
        self.assertEqual(MavenVersion("2.0.20").upgrade_type(curr), "patch")
        self.assertEqual(MavenVersion("2.1.0").upgrade_type(curr), "minor")
        self.assertEqual(MavenVersion("3.0.0").upgrade_type(curr), "major")
        self.assertEqual(MavenVersion("2.0.17").upgrade_type(curr), "none")
        self.assertEqual(MavenVersion("2.0.16").upgrade_type(curr), "none")


class TestPreferencesManager(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()
        self.config_path = os.path.join(self.tmp_dir, ".dependency_updates.json")

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def test_save_and_load(self):
        mgr = PreferencesManager(self.config_path)
        self.assertIsNone(mgr.get_preference("org.slf4j:slf4j-api"))

        mgr.set_preference("org.slf4j:slf4j-api", "patch_only", prop_name="slf4j.version")
        mgr.save()

        # Load fresh
        mgr2 = PreferencesManager(self.config_path)
        self.assertEqual(mgr2.get_preference("org.slf4j:slf4j-api"), "patch_only")
        self.assertEqual(mgr2.get_preference("other:dep", prop_name="slf4j.version"), "patch_only")


class TestPomUpdates(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()
        self.pom_path = os.path.join(self.tmp_dir, "pom.xml")
        pom_content = """<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <groupId>li.strolch</groupId>
    <artifactId>strolch</artifactId>
    <version>2.8.0-SNAPSHOT</version>
    <properties>
        <!-- logging -->
        <slf4j.version>2.0.20</slf4j.version>
        <logback.version>1.6.5</logback.version>
    </properties>
    <dependencies>
        <dependency>
            <groupId>org.openjdk.jol</groupId>
            <artifactId>jol-core</artifactId>
            <version>0.17</version>
            <scope>test</scope>
        </dependency>
    </dependencies>
</project>
"""
        with open(self.pom_path, 'w', encoding='utf-8') as f:
            f.write(pom_content)

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def test_update_property(self):
        updated = update_property_in_pom(self.pom_path, "slf4j.version", "2.0.20")
        self.assertTrue(updated)
        with open(self.pom_path, 'r', encoding='utf-8') as f:
            content = f.read()
        self.assertIn("<slf4j.version>2.0.20</slf4j.version>", content)
        self.assertIn("<!-- logging -->", content)

    def test_update_direct_dependency(self):
        updated = update_direct_dependency_in_pom(self.pom_path, "jol-core", "0.18")
        self.assertTrue(updated)
        with open(self.pom_path, 'r', encoding='utf-8') as f:
            content = f.read()
        self.assertIn("<version>0.18</version>", content)


class TestWorkflow(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()
        self.pom_path = os.path.join(self.tmp_dir, "pom.xml")
        pom_content = """<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <groupId>li.strolch</groupId>
    <artifactId>test-project</artifactId>
    <version>1.0.0-SNAPSHOT</version>
    <properties>
        <slf4j.version>2.0.20</slf4j.version>
        <annotation.version>2.1.1</annotation.version>
    </properties>
    <dependencies>
        <dependency>
            <groupId>org.slf4j</groupId>
            <artifactId>slf4j-api</artifactId>
            <version>${slf4j.version}</version>
        </dependency>
        <dependency>
            <groupId>jakarta.annotation</groupId>
            <artifactId>jakarta.annotation-api</artifactId>
            <version>${annotation.version}</version>
        </dependency>
    </dependencies>
</project>
"""
        with open(self.pom_path, 'w', encoding='utf-8') as f:
            f.write(pom_content)
        self.config_path = os.path.join(self.tmp_dir, ".dependency_updates.json")

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    @patch('update_dependencies.fetch_maven_central_versions')
    def test_stored_preferences_skip_and_patch_only(self, mock_fetch):
        # Setup preferences:
        # org.slf4j:slf4j-api -> patch_only (should update 2.0.17 to 2.0.20, skipping 2.1.0)
        # jakarta.annotation:jakarta.annotation-api -> skip (should stay on 2.1.1, skipping 3.0.0)
        mgr = PreferencesManager(self.config_path)
        mgr.set_preference("org.slf4j:slf4j-api", "patch_only", prop_name="slf4j.version")
        mgr.set_preference("jakarta.annotation:jakarta.annotation-api", "skip", prop_name="annotation.version")
        mgr.save()

        def fetch_side_effect(g, a, verbose=False):
            if a == 'slf4j-api':
                return ['2.0.16', '2.0.17', '2.0.18', '2.0.20', '2.1.0']
            if a == 'jakarta.annotation-api':
                return ['2.1.0', '2.1.1', '3.0.0']
            return []

        mock_fetch.side_effect = fetch_side_effect

        ret = run_updates(
            base_dir=self.tmp_dir,
            config_file=self.config_path,
            dry_run=False,
            non_interactive=True
        )
        self.assertEqual(ret, 0)

        with open(self.pom_path, 'r', encoding='utf-8') as f:
            content = f.read()

        # slf4j should be updated to latest patch 2.0.20 (skipping 2.1.0)
        self.assertIn("<slf4j.version>2.0.20</slf4j.version>", content)
        # jakarta.annotation should remain 2.1.1 (skipping 3.0.0)
        self.assertIn("<annotation.version>2.1.1</annotation.version>", content)

    @patch('update_dependencies.fetch_maven_central_versions')
    def test_dry_run_mode(self, mock_fetch):
        mock_fetch.return_value = ['2.0.18', '2.0.20']
        ret = run_updates(
            base_dir=self.tmp_dir,
            config_file=self.config_path,
            dry_run=True,
            non_interactive=True
        )
        self.assertEqual(ret, 0)
        with open(self.pom_path, 'r', encoding='utf-8') as f:
            content = f.read()
        # Should NOT be changed in dry-run
        self.assertIn("<slf4j.version>2.0.20</slf4j.version>", content)

    @patch('builtins.input')
    @patch('update_dependencies.fetch_maven_central_versions')
    def test_interactive_prompt_and_save_choice(self, mock_fetch, mock_input):
        # Targets are processed in sorted key order:
        # 1. prop:annotation.version (jakarta.annotation): choice '3' (skip), 'n' (don't remember)
        # 2. prop:slf4j.version (org.slf4j): choice '2' (patch only), 'y' (remember)
        mock_input.side_effect = ['3', 'n', '2', 'y']

        def fetch_side_effect(g, a, verbose=False):
            if a == 'slf4j-api':
                return ['2.0.18', '2.0.20', '3.0.0']
            if a == 'jakarta.annotation-api':
                return ['3.0.0']
            return []

        mock_fetch.side_effect = fetch_side_effect

        ret = run_updates(
            base_dir=self.tmp_dir,
            config_file=self.config_path,
            dry_run=False,
            non_interactive=False
        )
        self.assertEqual(ret, 0)

        with open(self.pom_path, 'r', encoding='utf-8') as f:
            content = f.read()

        # slf4j updated to patch 2.0.20
        self.assertIn("<slf4j.version>2.0.20</slf4j.version>", content)
        # jakarta.annotation stayed on 2.1.1
        self.assertIn("<annotation.version>2.1.1</annotation.version>", content)

        # Verify saved preference for slf4j
        mgr = PreferencesManager(self.config_path)
        self.assertEqual(mgr.get_preference("org.slf4j:slf4j-api"), "patch_only")
        self.assertIsNone(mgr.get_preference("jakarta.annotation:jakarta.annotation-api"))


if __name__ == '__main__':
    unittest.main()
