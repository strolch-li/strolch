#!/usr/bin/env python3
"""
Dependency version update script for Strolch.

This script checks Maven Central for updates to dependencies and plugins used in Strolch.
If a new major or minor version is detected, it informs the user and allows choosing
between upgrading to the latest version, staying on the current minor version (updating
only to the latest patch/update version), or skipping the update entirely.
User choices can be persisted in a configuration file for subsequent runs,
and skipped updates are logged accordingly.
"""

import argparse
import json
import os
import re
import sys
import urllib.request
import urllib.error
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple


MAVEN_CENTRAL_METADATA_URL = "https://repo1.maven.org/maven2/{group_path}/{artifact_id}/maven-metadata.xml"
DEFAULT_CONFIG_FILENAME = ".dependency_updates.json"


def is_prerelease(version_str: str) -> bool:
    """Check if a version string represents a pre-release or snapshot."""
    return bool(re.search(
        r'(?:^|[-._])(alpha|beta|rc|cr|milestone|preview|ea|snapshot|dev|incubating|m|b)\d*(?:[-._]|$)',
        version_str,
        re.IGNORECASE
    ))


class MavenVersion:
    """Represents and compares Maven artifact versions."""

    def __init__(self, raw: str):
        self.raw = raw.strip()
        self.is_prerelease = is_prerelease(self.raw)
        
        match = re.match(r'^(\d+(?:\.\d+)*)(.*)$', self.raw)
        if match:
            num_part, qual_part = match.groups()
            self.digits = [int(x) for x in num_part.split('.')]
            self.qualifier = qual_part.strip('.-_')
        else:
            nums = re.findall(r'\d+', self.raw)
            self.digits = [int(x) for x in nums] if nums else [0]
            self.qualifier = self.raw

    @property
    def major(self) -> int:
        return self.digits[0] if len(self.digits) > 0 else 0

    @property
    def minor(self) -> int:
        return self.digits[1] if len(self.digits) > 1 else 0

    @property
    def patch(self) -> int:
        return self.digits[2] if len(self.digits) > 2 else 0

    def is_same_major_minor(self, other: 'MavenVersion') -> bool:
        """Returns True if self and other share the same major and minor version numbers."""
        return self.major == other.major and self.minor == other.minor

    def upgrade_type(self, other: 'MavenVersion') -> str:
        """
        Determines the upgrade type from other (older) to self (newer).
        Returns 'major', 'minor', 'patch', or 'none'.
        """
        if self <= other:
            return "none"
        if self.major > other.major:
            return "major"
        if self.minor > other.minor:
            return "minor"
        return "patch"

    def __lt__(self, other: 'MavenVersion') -> bool:
        max_len = max(len(self.digits), len(other.digits))
        d1 = self.digits + [0] * (max_len - len(self.digits))
        d2 = other.digits + [0] * (max_len - len(other.digits))
        if d1 != d2:
            return d1 < d2
        if self.is_prerelease != other.is_prerelease:
            return self.is_prerelease
        return self.qualifier < other.qualifier

    def __le__(self, other: 'MavenVersion') -> bool:
        return self < other or self == other

    def __gt__(self, other: 'MavenVersion') -> bool:
        return not (self <= other)

    def __ge__(self, other: 'MavenVersion') -> bool:
        return not (self < other)

    def __eq__(self, other: 'MavenVersion') -> bool:
        return self.raw == other.raw

    def __hash__(self) -> int:
        return hash(self.raw)

    def __str__(self) -> str:
        return self.raw

    def __repr__(self) -> str:
        return f"MavenVersion('{self.raw}')"


@dataclass
class DependencyTarget:
    group_id: str
    artifact_id: str
    current_version_str: str
    property_name: Optional[str]  # e.g. "slf4j.version"
    pom_files: List[str]          # POM files referencing this


class PreferencesManager:
    """Manages persistent user choices for dependency upgrades."""

    def __init__(self, config_file: str):
        self.config_file = config_file
        self.preferences: Dict[str, dict] = {}
        self.load()

    def load(self):
        if os.path.exists(self.config_file):
            try:
                with open(self.config_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    self.preferences = data.get("preferences", {})
            except Exception as e:
                print(f"[WARN] Failed to load preferences from {self.config_file}: {e}", file=sys.stderr)
                self.preferences = {}
        else:
            self.preferences = {}

    def save(self):
        try:
            config_dir = os.path.dirname(self.config_file)
            if config_dir and not os.path.exists(config_dir):
                os.makedirs(config_dir, exist_ok=True)
            with open(self.config_file, 'w', encoding='utf-8') as f:
                json.dump({"preferences": self.preferences}, f, indent=2, sort_keys=True)
                f.write("\n")
        except Exception as e:
            print(f"[WARN] Failed to save preferences to {self.config_file}: {e}", file=sys.stderr)

    def get_preference(self, dep_key: str, prop_name: Optional[str] = None) -> Optional[str]:
        """
        Returns preference action: 'patch_only', 'skip', 'latest', or None.
        Checks dep_key (e.g. 'org.slf4j:slf4j-api') first, then property name if given.
        """
        if dep_key in self.preferences:
            return self.preferences[dep_key].get("action")
        if prop_name and f"prop:{prop_name}" in self.preferences:
            return self.preferences[f"prop:{prop_name}"].get("action")
        return None

    def set_preference(self, dep_key: str, action: str, prop_name: Optional[str] = None):
        entry = {"action": action}
        self.preferences[dep_key] = entry
        if prop_name:
            self.preferences[f"prop:{prop_name}"] = entry


def fetch_maven_central_versions(group_id: str, artifact_id: str, verbose: bool = False) -> List[str]:
    """Fetches all published versions for an artifact directly from Maven Central repository metadata."""
    group_path = group_id.replace('.', '/')
    url = MAVEN_CENTRAL_METADATA_URL.format(group_path=group_path, artifact_id=artifact_id)
    if verbose:
        print(f"[FETCH] {url}")
    
    req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0 (Strolch-Dependency-Updater)'})
    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            content = resp.read()
            root = ET.fromstring(content)
            version_nodes = root.findall('.//version')
            versions = [v.text.strip() for v in version_nodes if v.text and v.text.strip()]
            return versions
    except urllib.error.HTTPError as e:
        if e.code == 404:
            if verbose:
                print(f"[NOT FOUND] {group_id}:{artifact_id} on Maven Central", file=sys.stderr)
        else:
            print(f"[ERROR] HTTP {e.code} fetching {group_id}:{artifact_id}: {e}", file=sys.stderr)
        return []
    except Exception as e:
        print(f"[ERROR] Failed fetching {group_id}:{artifact_id}: {e}", file=sys.stderr)
        return []


def parse_pom_dependencies(pom_path: str) -> Tuple[Dict[str, str], List[dict]]:
    """
    Parses a POM file to extract properties and dependency/plugin references.
    """
    if not os.path.exists(pom_path):
        return {}, []

    try:
        tree = ET.parse(pom_path)
        root = tree.getroot()
    except Exception as e:
        print(f"[ERROR] Could not parse XML from {pom_path}: {e}", file=sys.stderr)
        return {}, []

    for elem in root.iter():
        if '}' in elem.tag:
            elem.tag = elem.tag.split('}', 1)[1]

    properties = {}
    prop_elem = root.find('properties')
    if prop_elem is not None:
        for child in prop_elem:
            properties[child.tag] = child.text.strip() if child.text else ''

    items = []
    # Dependencies
    for dep in root.findall('.//dependency'):
        g = dep.find('groupId')
        a = dep.find('artifactId')
        v = dep.find('version')
        if a is not None and v is not None and v.text:
            g_txt = g.text.strip() if (g is not None and g.text) else ''
            a_txt = a.text.strip()
            v_txt = v.text.strip()
            items.append({
                'type': 'dependency',
                'groupId': g_txt,
                'artifactId': a_txt,
                'version': v_txt,
                'pom': pom_path
            })

    # Plugins
    for plugin in root.findall('.//plugin'):
        g = plugin.find('groupId')
        a = plugin.find('artifactId')
        v = plugin.find('version')
        if a is not None and v is not None and v.text:
            g_txt = g.text.strip() if (g is not None and g.text) else 'org.apache.maven.plugins'
            a_txt = a.text.strip()
            v_txt = v.text.strip()
            items.append({
                'type': 'plugin',
                'groupId': g_txt,
                'artifactId': a_txt,
                'version': v_txt,
                'pom': pom_path
            })

    return properties, items


def discover_targets(base_dir: str) -> Tuple[str, Dict[str, str], Dict[str, DependencyTarget]]:
    """
    Discovers all dependency/plugin targets in base_dir.
    Returns:
      (root_pom_path, root_properties, {target_key: DependencyTarget})
    """
    root_pom = os.path.join(base_dir, "pom.xml")
    if not os.path.exists(root_pom):
        # Check if base_dir is already a pom.xml
        if base_dir.endswith(".xml") and os.path.exists(base_dir):
            root_pom = base_dir
            base_dir = os.path.dirname(base_dir) or "."
        else:
            raise FileNotFoundError(f"Root pom.xml not found at {root_pom}")

    root_properties, _ = parse_pom_dependencies(root_pom)

    # Collect all pom.xml files
    pom_files = []
    for root, _, files in os.walk(base_dir):
        if "pom.xml" in files:
            pom_files.append(os.path.join(root, "pom.xml"))

    targets: Dict[str, DependencyTarget] = {}

    for pom_file in pom_files:
        _, items = parse_pom_dependencies(pom_file)
        for item in items:
            g = item['groupId']
            a = item['artifactId']
            v = item['version']

            # Skip internal modules
            if g == 'li.strolch' or v == '${project.version}':
                continue

            prop_name = None
            current_ver = v
            if v.startswith('${') and v.endswith('}'):
                prop_name = v[2:-1]
                current_ver = root_properties.get(prop_name, '')
                if not current_ver:
                    continue
                # If target is controlled by a property, key by property name so grouped artifacts resolve together
                key = f"prop:{prop_name}"
            else:
                key = f"{g}:{a}"

            if key not in targets:
                targets[key] = DependencyTarget(
                    group_id=g,
                    artifact_id=a,
                    current_version_str=current_ver,
                    property_name=prop_name,
                    pom_files=[pom_file]
                )
            else:
                if pom_file not in targets[key].pom_files:
                    targets[key].pom_files.append(pom_file)

    return root_pom, root_properties, targets


def update_property_in_pom(pom_path: str, prop_name: str, new_version: str) -> bool:
    """Updates a property tag in a POM file while preserving structure and formatting."""
    with open(pom_path, 'r', encoding='utf-8') as f:
        content = f.read()

    pattern = rf'(<{re.escape(prop_name)}>)[^<]*(</{re.escape(prop_name)}>)'
    new_content, count = re.subn(pattern, rf'\g<1>{new_version}\g<2>', content)
    if count > 0:
        with open(pom_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
        return True
    return False


def update_direct_dependency_in_pom(pom_path: str, artifact_id: str, new_version: str) -> bool:
    """Updates a direct <version> tag for an artifact in a POM file."""
    with open(pom_path, 'r', encoding='utf-8') as f:
        content = f.read()

    pattern = rf'(<artifactId>{re.escape(artifact_id)}</artifactId>(?:(?!</dependency>|</plugin>).)*?<version>)[^<]*(</version>)'
    new_content, count = re.subn(pattern, rf'\g<1>{new_version}\g<2>', content, flags=re.DOTALL)
    if count > 0:
        with open(pom_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
        return True
    return False


def prompt_user_choice(
    dep_label: str,
    current_v: MavenVersion,
    latest_patch: MavenVersion,
    latest_overall: MavenVersion,
    upgrade_type: str
) -> Tuple[str, bool]:
    """
    Prompts the user interactively when a major/minor upgrade is available.
    Returns:
      (action, remember) where action is 'latest', 'patch_only', or 'skip'
    """
    print("\n" + "=" * 78)
    print(f"Dependency : {dep_label}")
    print(f"Current    : {current_v}")
    print(f"New Version: {latest_overall}  [{upgrade_type.upper()} UPGRADE]")
    if latest_patch > current_v:
        print(f"Patch Vers : {latest_patch}  (patch update in current {current_v.major}.{current_v.minor}.x series)")
    else:
        print(f"Patch Vers : {current_v}  (already on latest patch in {current_v.major}.{current_v.minor}.x series)")
    print("-" * 78)
    print("Options:")
    print(f"  [1] / [U] Upgrade to latest version ({latest_overall})")
    if latest_patch > current_v:
        print(f"  [2] / [P] Update only to latest patch/update version ({latest_patch})")
    else:
        print(f"  [2] / [P] Stay on current version (no newer patch available)")
    print(f"  [3] / [S] Skip update (stay on {current_v})")
    
    action = ""
    while not action:
        try:
            choice = input("Your choice [1=Upgrade, 2=Patch only, 3=Skip]: ").strip().lower()
        except (EOFError, KeyboardInterrupt):
            print("\nOperation aborted by user.")
            sys.exit(1)

        if choice in ('1', 'u', 'upgrade'):
            action = 'latest'
        elif choice in ('2', 'p', 'patch'):
            action = 'patch_only'
        elif choice in ('3', 's', 'skip'):
            action = 'skip'
        else:
            print("Invalid input. Please enter 1, 2, or 3.")

    remember = False
    try:
        rem_choice = input("Store this choice for future runs? [y/N]: ").strip().lower()
        remember = rem_choice in ('y', 'yes')
    except (EOFError, KeyboardInterrupt):
        pass

    return action, remember


def run_updates(
    base_dir: str,
    config_file: Optional[str] = None,
    dry_run: bool = False,
    non_interactive: bool = False,
    force_patch_only: bool = False,
    verbose: bool = False
) -> int:
    """
    Main workflow for checking and updating dependencies.
    """
    root_pom, root_properties, targets = discover_targets(base_dir)
    target_dir = os.path.dirname(root_pom) or "."
    
    if not config_file:
        config_file = os.path.join(target_dir, DEFAULT_CONFIG_FILENAME)

    prefs_mgr = PreferencesManager(config_file)

    print(f"Scanning dependencies in {root_pom}...")
    print(f"Found {len(targets)} external dependencies/plugins to check.")
    print(f"Preferences config file: {config_file}")
    if dry_run:
        print("[DRY-RUN MODE] No files will be modified.")
    if non_interactive:
        print("[BATCH / NON-INTERACTIVE MODE]")
    if force_patch_only:
        print("[PATCH-ONLY MODE] Major/minor upgrades will be restricted to patch updates.")
    print("-" * 78)

    updated_count = 0
    skipped_count = 0
    up_to_date_count = 0

    for key, target in sorted(targets.items()):
        dep_key = f"{target.group_id}:{target.artifact_id}"
        dep_label = f"{dep_key}" + (f" (property: {target.property_name})" if target.property_name else "")
        current_v = MavenVersion(target.current_version_str)

        all_raw_versions = fetch_maven_central_versions(target.group_id, target.artifact_id, verbose=verbose)
        if not all_raw_versions:
            if verbose:
                print(f"[SKIP] No versions found on Maven Central for {dep_label}")
            continue

        # Filter out pre-releases unless current version is a pre-release
        valid_versions = [
            MavenVersion(v) for v in all_raw_versions
            if current_v.is_prerelease or not is_prerelease(v)
        ]

        # Candidates strictly newer than current version
        newer_versions = [v for v in valid_versions if v > current_v]

        if not newer_versions:
            up_to_date_count += 1
            if verbose:
                print(f"[UP-TO-DATE] {dep_label}: {current_v}")
            continue

        latest_overall = max(newer_versions)
        patch_candidates = [v for v in newer_versions if v.is_same_major_minor(current_v)]
        latest_patch = max(patch_candidates) if patch_candidates else current_v
        upgrade_type = latest_overall.upgrade_type(current_v)

        # Stored user preference
        stored_pref = prefs_mgr.get_preference(dep_key, target.property_name)

        target_new_version: Optional[MavenVersion] = None

        if force_patch_only:
            if latest_patch > current_v:
                target_new_version = latest_patch
                if upgrade_type in ('major', 'minor'):
                    print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped (--patch-only active); updating to patch {latest_patch}.")
                    skipped_count += 1
            else:
                print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped (--patch-only active); staying on {current_v}.")
                skipped_count += 1
                continue

        elif upgrade_type in ('major', 'minor'):
            # Check if there is a stored preference
            if stored_pref == 'patch_only':
                if latest_patch > current_v:
                    target_new_version = latest_patch
                    print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped due to stored user choice (preference: patch_only); updating to patch {latest_patch}.")
                else:
                    print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped due to stored user choice (preference: patch_only); staying on {current_v}.")
                skipped_count += 1

            elif stored_pref == 'skip':
                print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped due to stored user choice (preference: skip); staying on {current_v}.")
                skipped_count += 1
                continue

            elif stored_pref == 'latest':
                target_new_version = latest_overall
                print(f"[INFO] {dep_label}: Upgrading to {latest_overall} (due to stored user choice: latest).")

            else:
                # No stored preference
                if non_interactive:
                    # In non-interactive batch mode without stored choice, default to patch update if available
                    if latest_patch > current_v:
                        target_new_version = latest_patch
                        print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped (non-interactive default: patch_only); updating to patch {latest_patch}.")
                    else:
                        print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped (non-interactive default: patch_only); staying on {current_v}.")
                    skipped_count += 1
                else:
                    action, remember = prompt_user_choice(
                        dep_label, current_v, latest_patch, latest_overall, upgrade_type
                    )
                    if remember:
                        prefs_mgr.set_preference(dep_key, action, target.property_name)
                        if not dry_run:
                            prefs_mgr.save()
                        print(f"[CONFIG] Stored preference '{action}' for {dep_label}")

                    if action == 'latest':
                        target_new_version = latest_overall
                    elif action == 'patch_only':
                        if latest_patch > current_v:
                            target_new_version = latest_patch
                            print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped by user choice; updating to patch {latest_patch}.")
                        else:
                            print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped by user choice; staying on {current_v}.")
                        skipped_count += 1
                    elif action == 'skip':
                        print(f"[SKIPPED] {dep_label}: New {upgrade_type} version {latest_overall} skipped by user choice; staying on {current_v}.")
                        skipped_count += 1
                        continue

        else:
            # Upgrade type is 'patch' only
            if stored_pref == 'skip':
                print(f"[SKIPPED] {dep_label}: Patch update to {latest_overall} skipped due to stored user choice (preference: skip).")
                skipped_count += 1
                continue
            target_new_version = latest_overall

        if target_new_version and target_new_version > current_v:
            new_v_str = target_new_version.raw
            if dry_run:
                print(f"[DRY-RUN] [UPDATE] {dep_label}: {current_v} -> {new_v_str}")
                updated_count += 1
            else:
                success = False
                if target.property_name:
                    success = update_property_in_pom(root_pom, target.property_name, new_v_str)
                else:
                    for pom_f in target.pom_files:
                        if update_direct_dependency_in_pom(pom_f, target.artifact_id, new_v_str):
                            success = True

                if success:
                    print(f"[UPDATED] {dep_label}: {current_v} -> {new_v_str}")
                    updated_count += 1
                else:
                    print(f"[ERROR] Failed to update POM for {dep_label}", file=sys.stderr)

    print("-" * 78)
    print(f"Summary: {updated_count} updated, {skipped_count} skipped/held back, {up_to_date_count} up-to-date.")
    return 0


def main():
    parser = argparse.ArgumentParser(
        description="Update Maven dependency and plugin versions from Maven Central with interactive major/minor controls."
    )
    parser.add_argument(
        "path",
        nargs="?",
        default=os.path.dirname(os.path.abspath(__file__)),
        help="Path to project directory or pom.xml (default: script directory)"
    )
    parser.add_argument(
        "--config",
        dest="config_file",
        default=None,
        help=f"Path to preferences configuration file (default: {DEFAULT_CONFIG_FILENAME} in project dir)"
    )
    parser.add_argument(
        "--dry-run", "-n",
        action="store_true",
        help="Check for updates without modifying POM files or preferences"
    )
    parser.add_argument(
        "--batch", "--non-interactive",
        dest="non_interactive",
        action="store_true",
        help="Run without interactive prompts, applying stored preferences or defaulting to patch updates"
    )
    parser.add_argument(
        "--patch-only",
        action="store_true",
        help="Update only patch/update versions in the current minor series; do not upgrade major/minor versions"
    )
    parser.add_argument(
        "--reset-preferences",
        action="store_true",
        help="Reset and clear all stored preferences"
    )
    parser.add_argument(
        "--list-preferences",
        action="store_true",
        help="Display stored preferences and exit"
    )
    parser.add_argument(
        "--set-preference",
        nargs=2,
        metavar=("DEPENDENCY", "ACTION"),
        help="Set preference for a dependency (e.g. --set-preference org.slf4j:slf4j-api patch_only|skip|latest)"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable verbose output"
    )

    args = parser.parse_args()

    # Determine config file path
    base_dir = args.path
    if os.path.isfile(base_dir):
        target_dir = os.path.dirname(base_dir) or "."
    else:
        target_dir = base_dir

    config_path = args.config_file or os.path.join(target_dir, DEFAULT_CONFIG_FILENAME)

    if args.reset_preferences:
        if os.path.exists(config_path):
            os.remove(config_path)
            print(f"Cleared preferences file: {config_path}")
        else:
            print(f"No preferences file found at: {config_path}")
        return 0

    if args.list_preferences:
        mgr = PreferencesManager(config_path)
        if not mgr.preferences:
            print(f"No preferences configured in {config_path}")
        else:
            print(f"Configured preferences in {config_path}:")
            for k, v in sorted(mgr.preferences.items()):
                print(f"  {k:45} : {v.get('action')}")
        return 0

    if args.set_preference:
        dep, act = args.set_preference
        if act not in ('patch_only', 'skip', 'latest'):
            print(f"Invalid action '{act}'. Allowed: patch_only, skip, latest", file=sys.stderr)
            return 1
        mgr = PreferencesManager(config_path)
        mgr.set_preference(dep, act)
        mgr.save()
        print(f"Stored preference: {dep} -> {act}")
        return 0

    return run_updates(
        base_dir=base_dir,
        config_file=config_path,
        dry_run=args.dry_run,
        non_interactive=args.non_interactive,
        force_patch_only=args.patch_only,
        verbose=args.verbose
    )


if __name__ == "__main__":
    sys.exit(main())
