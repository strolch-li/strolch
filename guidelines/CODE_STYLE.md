# Code Style

- **Language**: All code, documentation, comments, and commit messages must be in **English**. Even if requirements are
  provided in another language (e.g., German), the implementation and its documentation must be in English.
- **Localisations**: When adding localisations in German, always use Swiss German (e.g., never use the `ß` character).
- **Indentation**: Use **Tabs** for indentation, not spaces.
- **Code Layout**:
    - Consistent indentation is crucial for readability. Using tabs instead of spaces ensures that the code looks the
      same on all systems.
    - Keep lines of code under 160 characters.
    - When wrapping lines, break after a comma or an operator. Indent the new line with two tabs.
- **File Size**: Classes and files should ideally not exceed **1000 lines of code**. This is not a hard limit; use
  common sense and break up classes if it makes sense logically and improves maintainability.
- **License Header**: All source files should include the `atexxi Systems AG` copyright header. However, for Strolch
  code, which is open source, its original license must be respected.
- **Naming Conventions**:
    - Package names should be all lowercase, without underscores or other special characters. They should be short,
      meaningful, and based on the project's domain.
    - Classes/Interfaces: `PascalCase` and should be nouns or noun phrases.
    - Methods: `camelCase` and should be verbs or verb phrases.
    - Variables: `camelCase` and should be short and meaningful. Avoid single-letter variable names except for loop
      counters.
    - Constants: `UPPER_SNAKE_CASE` (e.g., `TYPE_PERSON`, `BAG_PARAMETERS`). Centrally defined in `ModelConstants.java`
      classes within each module where applicable.

## Best Practices for Classes, Interfaces, and Enums

### Use Records for data holder classes

Prefer using Java records for storing data holder classes.

```java
// Good
public record CustomerDTO(String name, String email) {
}
```

### Immutability

Prefer immutable classes whenever possible. Immutable objects are inherently thread-safe and make the code easier to
reason about.

### Use Interfaces

Program to interfaces, not implementations. This makes the code more flexible and easier to test.

```java
// Good
List<String> names = new ArrayList<>();
```

### Use Enums

Use enums instead of string constants or integer constants. Enums are type-safe and provide more readable and
maintainable code.

## Exception Handling

- **Catch Specific Exceptions**: Catch specific exceptions instead of `Exception` or `Throwable`.
- **Don't Ignore Exceptions**: Never ignore exceptions. If you catch an exception, either handle it or rethrow it.

## Concurrency

- **Use `java.util.concurrent`**: Prefer the high-level concurrency utilities in the `java.util.concurrent` package over
  low-level primitives like `wait()` and `notify()`.
- **Avoid `volatile` for Complex Operations**: Use `volatile` only for simple atomic operations. For more complex
  operations, use `java.util.concurrent.atomic` or locks.

## Use of `Optional`

- **Return Types**: Use `Optional` for return types when a method might not return a value. This makes the API clearer
  and helps prevent `NullPointerException`.
- **Don't Use for Fields or Parameters**: Do not use `Optional` for class fields or method parameters. For optional
  dependencies, use method overloading or a nullable annotation.

## Stream API Best Practices

- **Avoid Side Effects**: Avoid side effects in stream operations like `map()` and `filter()`.
- **Prefer Method References**: Prefer method references over lambdas when possible.

## Collections

- **Use the Right Collection**: Choose the right collection for the job. Use `List` for ordered collections, `Set` for
  unordered collections with no duplicates, and `Map` for key-value pairs.
- **Prefer `isEmpty()` over `size() == 0`**.
- **Return Empty Collections, Not Null**: Methods that return collections should return an empty collection instead of
  `null`.
- **Use Diamond Operator**: Use the diamond operator (`<>`) for generic type inference.
- **Use `for-each` loop**: Prefer the `for-each` loop for iterating over collections.

## Date and Time

Prefer using the Java 8 Date-Time API (`java.time.*`) over legacy `java.util.Date` and `java.util.Calendar`. The
`java.time` API is immutable, thread-safe, and more expressive.

## Strings

Use text blocks (`"""`), available since Java 15, for multi-line string literals (e.g., SQL, JSON, XML) instead of
concatenation or `\n` escapes.

## Debugging

- **Logging**: Use SLF4J with `LoggerFactory.getLogger(Class.class)`. When possible always use SLF4J, never `System.out`
  or `printStackTrace` and related methods.
- **Strolch Transactions**: Ensure transactions are properly committed or rolled back.

## Documentation

When a new feature is implemented, the `README.md` file in the root of the repository (not in atx-dev directly) must be
updated (or created if it doesn't exist) to document the feature for the end user. This documentation should include
instructions on how to use and/or configure the new feature.

Furthermore, a technical specification of the feature must also be documented in a `docs/` directory at the root of the
repository (or within the module if specific to it).

## Vanilla JavaScript Guidelines

These guidelines apply only to browser applications written in vanilla JavaScript, in both new and existing projects.
They do not apply to Polymer projects.
Use native ES modules and separate application coordination, UI, API access, and reusable utilities.
Adapt directory names and integration contracts to the project; the examples below are not required file names.
Apply these rules to new code and code changed for a task; do not rewrite unrelated files to achieve compliance.
Existing large files and duplicated patterns are not templates for new code.

### Module Responsibilities

- Use native `import`/`export` with explicit relative `.js` paths. Do not introduce a frontend framework, bundler,
  dependency, or generic application framework without agreement.
- Keep the application entry point responsible for startup, routing, navigation, and application-wide coordination.
  Feature-specific rendering, forms, and calculations belong outside it.
- Keep page or view modules as coordinators: own page state, call APIs, compose UI, and handle page actions.
  Define explicit initialization, rendering, and cleanup contracts; preserve existing integrations when extending a project.
- Keep API modules focused on endpoint URLs, request parameters, payloads, and response contracts.
  Use a shared transport layer for authentication and request/error behavior; do not duplicate `fetch` logic in views.
  API modules must not manipulate the DOM or show dialogs.
- Group reusable formatting and other focused helpers separately from UI; keep translation resources together.
  Reuse existing formatters, localization helpers, notifications, and input widgets before adding equivalents.
- Keep feature-only helpers beside their feature, using a feature subdirectory when needed.
  Put UI shared by multiple features in a shared components directory, such as `components/`, when first needed.
  Existing shared widgets do not need to be moved merely to match this layout.
- Dependencies should flow from pages to components, API modules, and utilities, not back to pages or the application entry point.
  Avoid circular imports. Pass explicit data and callbacks rather than letting helpers reach through the whole app.

### File and Function Size

- Aim for JavaScript modules of roughly 300–500 lines or fewer, including templates; small files need no minimum size.
  At about 600 lines, review the responsibilities and extract cohesive pieces before adding substantial behavior.
  These are review thresholds, not mechanical limits; explain a justified exception in the change summary.
- Prefer functions of about 100 lines or fewer, doing one identifiable job. Long templates count as complexity too.
  Split when a function mixes loading, transformation, rendering, event binding, and saving.
- Extract along behavior boundaries: a form editor, search filter, table renderer, or pure calculation.
  Do not merely move hundreds of lines into a catch-all `helpers.js` or `templates.js` file.
- A page's `render()` should read as composition of named sections rather than contain the entire screen and every dialog.
  A cohesive component may own its markup, event handlers, and local state together.
- Do not create one-line forwarding modules or split tightly coupled code solely to meet a line count.

### Reuse Without Over-Abstraction

- Search for existing implementations before adding formatting, validation, dialogs, filters, or request handling.
  Extend a suitable helper instead of copying it into another view.
- When the same behavior is needed in a second place, assess extraction; by a third use, normally share it.
  Small coincidental similarities may remain separate when the behaviors are likely to evolve differently.
- Give shared code a narrow, explicit contract: input data, options, output, callbacks, and cleanup ownership.
  Prefer composition and small functions over base-view inheritance, mode flags, and configurable CRUD engines.
- Keep business rules and derived-data transformations in pure, named functions when feasible.
  Do not mix DOM access or network requests into calculations; server-side rules remain authoritative.
- Keep a helper private until another module actually needs it. Export the smallest useful surface.
  Use a consistent export convention, such as default exports for primary classes and named exports for focused helpers.

### Syntax and Readability

- Use tabs, semicolons, single-quoted strings, and template literals for interpolation or readable markup.
  Keep lines below 160 characters and wrap continuations with two tabs unless the project defines a different style.
  Do not reformat an entire legacy space-indented file as part of an unrelated change.
- Use `const` by default, `let` only for reassignment, and never introduce `var`.
  Prefer strict equality, explicit conditions, early returns, and shallow nesting.
- Use `PascalCase` for classes and matching class files, `camelCase` for functions and variables,
  and `UPPER_SNAKE_CASE` for fixed shared constants. Name booleans with `is`, `has`, or `can` where natural.
- Initialize instance state explicitly. Avoid new globals and hidden mutable module state except for deliberately
  application-wide services with clear ownership.
- Use concise comments for reasons, constraints, and non-obvious behavior, not narration of the code.
  Add JSDoc for non-obvious shared contracts or data shapes, not every trivial private method.

### DOM, Styling, and Lifecycle

- Scope selectors to the view/component root; reserve document-wide selectors for application-owned UI.
  Use `addEventListener`, not inline event attributes. Delegate repeated row actions from a stable container when useful.
- Use `textContent` and DOM properties for user/server values. Do not interpolate untrusted strings into `innerHTML`.
  If HTML interpolation is unavoidable, use a shared, context-appropriate escaping implementation; HTML escaping
  alone does not make URLs, CSS, or JavaScript safe. Keep templates primarily static.
- Put reusable presentation in CSS classes rather than copying inline styles across templates.
  Preserve semantic controls, labels, keyboard operation, and dialog focus behavior when extracting components.
- Make setup idempotent where it can run again. Avoid duplicate listeners and reinitializing widgets unnecessarily.
- Code that creates timers, observers, global listeners, or pending requests owns their cleanup.
  If introducing a teardown method, also wire it into the actual route/component removal path; do not assume
  ordinary page classes receive Web Component lifecycle callbacks.
- Keep state in the owning page/component and pass it explicitly. Avoid using DOM text as a second data model.

### Standard Modal Dialogs

- Never use JavaScript's `alert()` or `window.alert()`, including for debugging or as a fallback.
  Provide shared application dialogs for information, errors, and confirmation; use the confirmation dialog instead of `window.confirm()`.
- All these dialogs must be modal: background content must be inert and unavailable to pointer and keyboard interaction until dismissed.
  Prefer native `<dialog>` opened with `showModal()`; a custom implementation must provide equivalent modality and accessibility.
- Use a consistent content contract: a required `title`, a primary `message`, and optional `details`.
  These correspond to title, line1, and line2, but allow wrapping and multiple paragraphs rather than imposing fixed visual lines.
  Render content safely as text and localize content and button labels through the application's localization mechanism.
- Information and error dialogs must have a visible Close button. Confirmation dialogs must have Cancel and a primary confirmation button,
  defaulting to OK; prefer an explicit action label such as Delete or Save when it makes the consequence clearer.
- Give each dialog an accessible name linked to its title and associate simple message text as its accessible description.
  Use dialog semantics normally; reserve `alertdialog` semantics for urgent messages that require immediate attention and a response.
- Move focus into the dialog when opened, contain Tab and Shift+Tab navigation within it, and restore focus to the invoking control
  (or a logical fallback) when closed. For destructive confirmations, initially focus Cancel rather than the destructive action.
- Escape must close information/error dialogs and cancel confirmation dialogs. Any other dismissal path must also mean cancellation,
  never approval; do not dismiss confirmation dialogs on backdrop clicks. Only explicit activation of the primary button confirms an action.
- Expose confirmation results asynchronously, for example as a `Promise<boolean>`, and wait for approval before performing the action.
  Resolve each result once, avoid stacked dialogs, and clean up listeners and pending results when a dialog is removed.

### Asynchronous Work and Errors

- Prefer `async`/`await` with explicit error handling at the UI action boundary. Never silently swallow failures.
  Reuse shared notification UI and localized messages; keep transport handling in the shared request layer rather than repeating it.
- Represent loading, empty, success, and error states. Disable duplicate submissions while saving and restore
  controls in `finally`. Keep entered data available after a failed request.
- Prevent older responses from overwriting newer filters or a different route, using request identifiers or cancellation.
  Treat intentional cancellation differently from a user-visible failure.
- Preserve concurrency headers such as `If-Match` and handle conflicts rather than blindly overwriting newer data.
  UI role checks improve usability but never replace backend authorization.

### Localization, Dates, and Numbers

- Use the project's localization mechanism for visible text and maintain matching keys in all supported locale files.
  Follow the spelling, terminology, and formatting conventions of each supported locale.
- Always format user-facing dates, times, and numbers (including currencies and percentages) through shared helper functions.
  Reuse or extend existing helpers; never format these values directly in pages, components, or templates.
- Centralize locale selection and formatting defaults in those helpers, using `Intl.DateTimeFormat` and `Intl.NumberFormat` where appropriate.
  Keep date/time styles, timezone policy, decimal precision, and grouping conventions centrally configurable so localization changes
  do not require edits throughout the UI. Pass explicit currency codes or other domain-specific options when needed.
- Keep direct `Intl` formatting, `toLocaleString()` variants, and manual display formatting inside the shared helpers.
  Do not hard-code locale identifiers, date patterns, decimal separators, or grouping rules at call sites.
- Reuse shared date input widgets and parsing helpers. Keep localized display formatting separate from canonical API/storage values
  and machine-readable input values; do not send localized display strings to APIs or use them for calculations.
- Distinguish date-only values from timestamps. Keep calendar dates as date-only strings where the API expects them;
  do not use `toISOString().split('T')[0]` to derive a local calendar date, since UTC conversion can change the day.
  Convert timestamps explicitly at API boundaries and cover timezone/day-boundary behavior when changing date logic.

### Verification and Review

- For behavior changes, add or update focused regression tests proportional to risk, especially for shared helpers,
  date calculations, validation, and failure paths. Preserve behavior when extracting code.
- Use the project's existing test suites and conventions where available.
  Source-text assertions alone do not prove browser behavior; also exercise affected interactions in the browser.
  Do not add a new JavaScript test toolchain without agreement.
- When UI behavior changes, check success, empty/error states, repeated rendering/navigation, and relevant roles/locales.
  For asynchronous filters and forms, also check rapid changes and duplicate submissions.
- Before completing a change, check module size, duplicated behavior, dependency direction, safe rendering,
  cleanup ownership, and translation coverage. Report what was actually verified and any remaining limitations.
- Documentation-only changes do not require builds or tests. These guidelines themselves do not require an immediate
  refactoring of existing JavaScript files.
