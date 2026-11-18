### Strolch Job Handler

The `StrolchJobsHandler` manages background tasks and scheduled jobs within the Strolch Agent.

#### StrolchJob

A `StrolchJob` is a background task that can be executed periodically or on demand. It provides methods for:
-   **Scheduling**: Define execution intervals using delays or CRON expressions.
-   **Execution**: Logic is implemented in the `execute` method.
-   **Lifecycle**: Jobs are registered with the `StrolchJobsHandler`.
-   **Privileges**: Jobs run with specific privileges, typically as a system user or agent.

#### Implementing a Job

To create a new job, extend the `StrolchJob` class and implement the `doExecute` method (or similar, depending on the base class).

```java
public class MyJob extends StrolchJob {
    public MyJob(StrolchAgent agent, String id, String name, JobMode mode) {
        super(agent, id, name, mode);
    }

    @Override
    protected void doExecute(PrivilegeContext ctx) throws Exception {
        runAsAgent(tx -> {
            // Perform background work in a transaction
            tx.commitOnClose();
        });
    }
}
```

#### Job Modes

-   **`JobMode.Manual`**: The job must be triggered manually.
-   **`JobMode.Recurring`**: The job runs periodically based on a defined delay.
-   **`JobMode.Cron`**: The job runs according to a CRON expression.

#### Scheduling a Job

Jobs can be scheduled programmatically or via configuration.

```java
StrolchJobsHandler jobsHandler = agent.getComponent(StrolchJobsHandler.class);
MyJob job = jobsHandler.register(MyJob.class);
job.setDelay(1, TimeUnit.MINUTES, 5, TimeUnit.MINUTES);
job.schedule();
```

#### Model-based Configuration

Jobs can also be configured as Strolch `Resource` elements in the model (e.g., in XML files). This allows for dynamic job registration and configuration without code changes.

A job resource must have the `Type` set to `StrolchJob`.

**Parameter Bag: `parameters`**

| Parameter Id | Type | Interpretation | Uom | Description |
| :--- | :--- | :--- | :--- | :--- |
| `className` | `String` | - | - | The fully qualified Java class name of the job. |
| `mode` | `String` | `Enumeration` | `JobMode` | The execution mode: `Manual`, `Recurring`, or `Cron`. |
| `cron` | `String` | - | - | The CRON expression (e.g., `0 0/5 * * * ?`). Required for `Cron` mode. |
| `delay` | `Integer` | `TimeUnit` | `SECONDS`, `MINUTES`, etc. | The delay between executions. Required for `Recurring` mode. |
| `initialDelay` | `Integer` | `TimeUnit` | `SECONDS`, `MINUTES`, etc. | The initial delay before the first execution. |
| `startDate` | `Date` | - | - | The date and time when the job should start. |

**Custom Parameters**

Additional `ParameterBag` elements can be added to the resource to provide job-specific configuration. These can be accessed within the job's `doExecute` method by retrieving the resource from the transaction.

**Example XML Configuration**

```xml
<Resource Id="expiredProductsJob" Name="Expired Products Job" Type="StrolchJob">
    <ParameterBag Id="parameters" Name="Parameters" Type="Parameters">
        <Parameter Id="className" Name="Class Name" Type="String" Value="ch.atexxi.esybox.core.jobs.ExpiredProductsJob"/>
        <Parameter Id="mode" Name="Job Mode" Type="String" Interpretation="Enumeration" Uom="JobMode" Value="Recurring"/>
        <Parameter Id="delay" Name="Delay" Type="Integer" Interpretation="TimeUnit" Uom="MINUTES" Value="30"/>
        <Parameter Id="initialDelay" Name="Initial Delay" Type="Integer" Interpretation="TimeUnit" Uom="SECONDS" Value="10"/>
    </ParameterBag>
    <ParameterBag Id="relations" Name="Relations" Type="Parameters">
        <Parameter Id="locations" Name="Location" Type="StringList" Interpretation="Resource-Ref" Uom="Location" Value="WardA"/>
    </ParameterBag>
</Resource>
```

#### Monitoring

The `StrolchJobsHandler` provides information about job status, last execution time, duration, and any exceptions that occurred. This information is often exposed via REST APIs for monitoring purposes.

#### Resource Monitor Job

The `li.strolch.job.ResourceMonitorJob` monitors filesystem roots, physical memory, system CPU, JVM heap, and process CPU.
It reads its configuration and stores its state in the realm containing the job resource.

**Configuration**

Configure a `StrolchJob` resource as described above, with `className` set to `li.strolch.job.ResourceMonitorJob` and a
recurring or CRON schedule. Add the following parameters to the resource's `parameters` bag. All are required except
`diskThresholdOverrides` and `verbose`.

| Parameter Id | Type | Description |
| :--- | :--- | :--- |
| `monitorId` | `String` | Nonblank, stable identifier for this monitor. Keep it unchanged across reloads and restarts; use a distinct ID for each deployment sharing a realm. |
| `notificationMode` | `String` | `ALWAYS` reports every run; `THRESHOLD` reports breaches, reminders, and recoveries. |
| `verbose` | `Boolean` | Optional; defaults to `false`. When `true`, logs sampled metric status and the reason a transition, reminder, or report is or is not produced, plus pending retries and delivery/acknowledgement outcomes. |
| `recipients` | `StringList` | At least one nonblank email recipient. |
| `reminderInterval` | `String` | Positive ISO-8601 duration, such as `PT1H`; required even in `ALWAYS` mode. |
| `diskThresholdPercent` | `String` or number | Default disk utilization threshold. |
| `diskThresholdOverrides` | `String` | Optional JSON object mapping exact filesystem root paths to numeric percentage thresholds, for example `{"/":90}`. Other roots use the default. |
| `systemMemoryThresholdPercent` | `String` or number | Physical memory utilization threshold. |
| `systemCpuThresholdPercent` | `String` or number | System CPU utilization threshold. |
| `jvmHeapThresholdPercent` | `String` or number | JVM heap utilization threshold. |
| `processCpuThresholdPercent` | `String` or number | JVM process CPU utilization threshold. |

Every threshold, including overrides, must be finite and greater than 0 and at most 100. Override paths must be nonblank;
malformed JSON, nonnumeric values, invalid thresholds, blank recipients, and invalid durations are rejected.

Verbose logging is disabled by default. When enabled, it logs at INFO level and includes sampled percentages, thresholds,
and the inputs used to calculate them: disk used/free/total/usable bytes, physical-memory used/free/total bytes, heap
used/free/max bytes, and raw system/process CPU ratios as returned by the JVM. Invalid non-null inputs are logged with
their sampled values, while missing inputs are labeled unavailable rather than replaced with zero. Logs may include
filesystem paths and operational measurements; restrict access to application logs accordingly. Recipients are not
logged, and measurement data is not placed in email headers or the plaintext mail wrapper.

**Example XML Configuration**

```xml
<Resource Id="resourceMonitor" Name="Resource Monitor" Type="StrolchJob">
    <ParameterBag Id="parameters" Name="Parameters" Type="Parameters">
        <Parameter Id="className" Name="Class Name" Type="String" Value="li.strolch.job.ResourceMonitorJob"/>
        <Parameter Id="mode" Name="Job Mode" Type="String" Interpretation="Enumeration" Uom="JobMode" Value="Recurring"/>
        <Parameter Id="delay" Name="Delay" Type="Integer" Interpretation="TimeUnit" Uom="MINUTES" Value="5"/>
        <Parameter Id="initialDelay" Name="Initial Delay" Type="Integer" Interpretation="TimeUnit" Uom="SECONDS" Value="10"/>
        <Parameter Id="monitorId" Name="Monitor ID" Type="String" Value="host-monitor-1"/>
        <Parameter Id="notificationMode" Name="Notification Mode" Type="String" Value="THRESHOLD"/>
        <Parameter Id="recipients" Name="Recipients" Type="StringList" Value="ops@example.com"/>
        <Parameter Id="reminderInterval" Name="Reminder Interval" Type="String" Value="PT1H"/>
        <Parameter Id="diskThresholdPercent" Name="Disk Threshold" Type="String" Value="80"/>
        <Parameter Id="diskThresholdOverrides" Name="Disk Overrides" Type="String" Value="{&quot;/&quot;:90}"/>
        <Parameter Id="systemMemoryThresholdPercent" Name="System Memory Threshold" Type="String" Value="85"/>
        <Parameter Id="systemCpuThresholdPercent" Name="System CPU Threshold" Type="String" Value="90"/>
        <Parameter Id="jvmHeapThresholdPercent" Name="JVM Heap Threshold" Type="String" Value="85"/>
        <Parameter Id="processCpuThresholdPercent" Name="Process CPU Threshold" Type="String" Value="90"/>
        <Parameter Id="verbose" Name="Verbose" Type="Boolean" Value="false"/>
    </ParameterBag>
</Resource>
```

**Notifications and State**

A value at or above its threshold is a breach; a later known value below the threshold is a recovery. In `THRESHOLD`
mode, a continuing breach generates a reminder on a subsequent sampling run once `reminderInterval` has elapsed since
the last successful breach or reminder notification. In `ALWAYS` mode, each sample produces a report, including any
transitions, without separate reminders. Events from one sample are consolidated into one report.

Unavailable measurements and missing roots are not treated as healthy and do not cause recovery. A later valid sample
below the threshold can recover a previously breached metric. There is no hysteresis, so values oscillating around a
threshold can generate repeated breach and recovery events.

State and pending snapshots are stored in the job's source realm, keyed by realm, job ID, and `monitorId`. A persistent
realm is required for state and pending notifications to survive restarts; a transient realm loses them. Protect realm
storage and access because pending snapshots contain operational measurements. Delivery retries pending
reports in order and acknowledges them only after a successful synchronous send. Delivery is at least once, not exactly
once: a crash after sending but before acknowledgement can result in duplicate email. Overlapping executions for the same
key are serialized within one JVM only; run a single active deployment per monitor ID, including across JVMs.

**Report Format and Delivery**

Each report is recorded in `OperationsLog` with a generic message and sent as a signed, encrypted
`resource-monitor.json.asc` attachment. Decrypt the OpenPGP attachment to obtain the `application/json` snapshot. The
email content and attachment are encrypted, but the subject and standard mail metadata, including sender and recipients,
remain visible; resource measurements must not be placed in the subject or plaintext wrapper. OS, JDK and application
metadata are added when delivery occurs, so a retried report retains its original sampled metrics but describes the
runtime that sends the retry.

The versioned JSON snapshot has these root fields:

| Field | Description |
| :--- | :--- |
| `schemaVersion` | Report schema version. |
| `monitorId`, `jobId`, `realm` | Monitor, job, and source realm identifiers. |
| `sampledAt` | Sampling time as a UTC ISO-8601 timestamp. |
| `notificationMode` | `ALWAYS` or `THRESHOLD` for the report. |
| `events` | Array of events with `metricKey` and `type`: `BREACH`, `REMINDER`, or `RECOVERY`; may be empty. |
| `metrics` | Array of metric snapshots. |
| `operatingSystem` | Object with OS `name` and `version`. |
| `jdk` | Object with Java `vendor`, runtime `type`, and `version`. |
| `application` | Object with configured application `name` and `environment` from `AgentVersion`, and deployed application `version` from `StrolchAgent.getVersion().getAppVersion()`. |
| `strolchAgent` | Separate Strolch agent `groupId`, `artifactId`, and `version` from `AgentVersion`; this is not the deployed application version. |

Each metric has a stable `key`, `unit` (`percent`), nullable `value`, numeric `threshold`, and `status` (`HEALTHY`,
`BREACHED`, or `UNAVAILABLE`). Relevant byte counters are `totalBytes`, `freeBytes`, `usableBytes`, and `usedBytes`;
unavailable or inapplicable counters may be null. Disk keys include the root path as `disk:<path>`; other keys are
`systemMemory`, `systemCpu`, `jvmHeap`, and `processCpu`.

Before enabling delivery, configure a working SMTP `MailHandler`, valid recipients, signing key and password, and public
encryption keys for all intended recipients; see [Mail Handler](mail.md). Verify that recipients can decrypt the reports.
Encryption must be enabled and delivery must fail closed rather than fall back to plaintext.

**Sampling Limits**

Disk and physical memory utilization use `(total - free) / total`; disk reports also include usable space. Heap
utilization uses `used / max`. Filesystems are discovered with `File.listRoots()`: on Unix this usually exposes only `/`,
not every mounted filesystem. CPU values come from the JVM/platform's recent system and process CPU utilization samples,
scaled to percentages; they are not load averages. Unsupported or invalid readings, inaccessible roots, zero totals, and
an undefined heap maximum yield unavailable values rather than zero.
