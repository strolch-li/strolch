# Execution Framework

The Strolch Execution Framework is a reactive engine for executing hierarchical business processes defined as `Activity` and `Action` elements.

## Core Components

### ExecutionHandler

The `ExecutionHandler` is the central component that manages the state of activities in execution. It listens for state changes and triggers the appropriate policies to advance the execution.

- `EventBasedExecutionHandler`: The standard implementation that reacts to state changes in the model.
- `Controller`: Manages the execution for a specific realm.

### Activities and Actions

- **Activity**: A container for other Activities or Actions. It defines a sequence (or parallel execution) of steps.
- **Action**: An atomic step in an activity. It has a state (Created, Planned, Execution, Stopped, Warning, Error, Executed, Closed).

### Policies

The behavior of the execution framework is highly configurable through policies:

- **ExecutionPolicy**: Defines how an `Action` is executed (e.g., `SimpleExecution`, `DurationExecution`).
- **PlanningPolicy**: Defines how an `Activity` or `Action` is planned before execution.
- **ConfirmationPolicy**: Defines how the completion of an `Action` is confirmed.

## Execution Flow

1. **Planning**: An `Activity` is transitioned to the `Planned` state. This may involve resource reservation or time calculations.
2. **Execution**: The `ExecutionHandler` identifies actions that are ready to run and transitions them to `Execution` state.
3. **Completion**: Once the underlying task is done, the action is transitioned to `Executed` (and eventually `Closed`).

## State Machine

The execution framework follows a strict state machine for `Action` elements. Transitions are usually handled by the `ExecutionHandler` or specific `Services`.

## Configuration

Execution policies are assigned to `Action` elements via `PolicyDefs` in the Strolch model (XML or programmatic).

```xml
<Action Id="action_1" Type="Produce" Name="Production Step">
    <Policy Value="li.strolch.execution.policy.SimpleExecution" Type="ExecutionPolicy" />
</Action>
```

## Services

Key services for interacting with the execution framework:
- `StartActivityExecutionService`
- `ExecuteActionService`
- `SetActionToExecutedService`
- `SetActionToErrorService`
