# Temporary File Retention Specification

## Overview
The Strolch framework provides a mechanism to write temporary files to a designated `tempPath`. To prevent this directory from growing indefinitely, a background job is implemented to clear old data based on configurable retention periods.

## Configuration
Retention periods are configured in the `Runtime` section of the `strolch.xml` configuration file using ISO-8601 duration strings (e.g., `P7D` for 7 days, `PT12H` for 12 hours).

### Default Retention
The default retention period applies to any file in the `tempPath` that does not match a prefix-specific configuration.
- **Property**: `temp.retention.default`
- **Default Value**: `P3M` (3 months)

### Prefix-Specific Retention
Different retention periods can be defined for subdirectories (prefixes) within the `tempPath`.
- **Property**: `temp.retention.<prefix>`
- **Example**: `temp.retention.import` with value `P1D` will retain files in the `temp/import` directory for 1 day.

### Minimum Files to Keep
Optionally, you can specify a minimum number of files to keep for a prefix, regardless of their age.
- **Property**: `temp.retention.keep.<prefix>`
- **Default Property**: `temp.retention.keep.default`
- **Behavior**: If specified, the retention job will always keep the newest `n` files. Only files beyond this count will be checked against the retention duration.
- **Default Value**: `0` (no minimum)

### Simulation Mode
By default, the job runs in simulation mode and only logs what would be deleted. Deletion must be explicitly enabled.
- **Property**: `temp.retention.delete.enabled`
- **Behavior**: If `true`, files are actually deleted. If `false` (default), the job only logs which files and directories would be deleted.

### Arbitrary Temporary Paths (`ClearTempPathsJob`)
In addition to the standard agent temporary path, arbitrary external temporary paths can be configured for cleanup.
- **Property**: `clear.temp.path.ids` (comma-separated list of path IDs)
- **Path Property**: `clear.temp.path.<pathId>.path` (absolute or relative path to clean)
- **Retention Property**: `clear.temp.path.<pathId>.retention` (ISO-8601 duration, default `P90D`)
- **Keep Property**: `clear.temp.path.<pathId>.keep` (minimum number of files to keep, default `0`)

## Implementation Details

### RuntimeConfiguration
The `RuntimeConfiguration` class is extended with constants and methods to retrieve retention settings:
- `getTempRetention(String prefix)`: Returns the `java.time.Duration` for the given prefix, falling back to the default retention if not specified.
- `getTempRetentionKeep(String prefix)`: Returns the number of files to keep for the given prefix, falling back to the default keep if not specified.
- `isTempRetentionDeleteEnabled()`: Returns whether deletion is enabled.

### ClearTempPathJob and ClearTempPathsJob
Two jobs are implemented to perform the cleanup:
1. `ClearTempPathJob`: Cleans the agent's standard `tempPath` (subdirectories/prefixes and root files).
2. `ClearTempPathsJob`: Cleans arbitrary temporary paths configured via `clear.temp.paths`.
- **Frequency**: Runs periodically (e.g. daily/hourly).
- **Behavior**:
    1. Iterates through all files and subdirectories in the configured temp path(s).
    2. Determines the applicable retention period and the number of files to keep.
    3. Collects all files recursively, sorts them by last modified date (newest first), and preserves the first `n` files.
    4. Deletes files older than the retention period and cleans up empty subdirectories.

## Example Configuration in strolch.xml
```xml
<Runtime>
    <tempPath>temp</tempPath>
    <properties>
        <!-- enable removal -->
        <temp.retention.delete.enabled>true</temp.retention.delete.enabled>
        
        <!-- Default retention: 3 months -->
        <temp.retention.default>P90D</temp.retention.default>
        <!-- Default keep: 0 files -->
        <temp.retention.keep.default>0</temp.retention.keep.default>
        
        <!-- Prefix-specific retention for 'import': 1 day, keep 10 files -->
        <temp.retention.import>P1D</temp.retention.import>
        <temp.retention.keep.import>10</temp.retention.keep.import>
        
        <!-- Prefix-specific retention for 'reports': 30 days -->
        <temp.retention.reports>P30D</temp.retention.reports>
        
        <!-- Arbitrary temporary paths cleanup -->
        <clear.temp.path.ids>tmp,custom_exports</clear.temp.path.ids>
        
        <clear.temp.path.tmp.path>/var/log/app/tmp</clear.temp.path.tmp.path>
        <clear.temp.path.tmp.retention>P7D</clear.temp.path.tmp.retention>
        <clear.temp.path.tmp.keep>5</clear.temp.path.tmp.keep>
        
        <clear.temp.path.custom_exports.path>/tmp/custom_exports</clear.temp.path.custom_exports.path>
        <clear.temp.path.custom_exports.retention>P7D</clear.temp.path.custom_exports.retention>
        <clear.temp.path.custom_exports.keep>5</clear.temp.path.custom_exports.keep>

    </properties>
</Runtime>
```
