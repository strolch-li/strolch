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

## Implementation Details

### RuntimeConfiguration
The `RuntimeConfiguration` class is extended with constants and methods to retrieve retention settings:
- `getTempRetention(String prefix)`: Returns the `java.time.Duration` for the given prefix, falling back to the default retention if not specified.
- `getTempRetentionKeep(String prefix)`: Returns the number of files to keep for the given prefix, falling back to the default keep if not specified.
- `isTempRetentionDeleteEnabled()`: Returns whether deletion is enabled.

### ClearTempPathJob
A new `StrolchJob` called `ClearTempPathJob` is implemented to perform the cleanup:
- **Frequency**: Runs once a day by default (configurable).
- **Behavior**:
    1. Iterates through all files and subdirectories in the `tempPath`.
    2. For each subdirectory (prefix), it determines the applicable retention period and the number of files to keep.
    3. It collects all files in the prefix directory recursively.
    4. It sorts the files by last modified date (newest first).
    5. It keeps the first `n` files (where `n` is the "keep" count).
    6. For the remaining files, it deletes any file whose last modified time is older than the retention period.
    7. Recursively deletes empty subdirectories.
    8. Files directly in the `tempPath` root are cleaned using the default retention and keep settings.

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
    </properties>
</Runtime>
```
