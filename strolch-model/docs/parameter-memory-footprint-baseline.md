# Parameter Memory Footprint Baseline

`strolch-model` contains an opt-in JUnit 4 baseline for measuring the current generic Parameter model before low-risk or compact storage optimizations are implemented.

Run it from the repository root with:

```bash
mvn -pl strolch/strolch-model -Dtest=ParameterMemoryFootprintBaselineTest \
	-DparameterMemoryFootprintBaseline.enabled=true test
```

The default dataset creates `100000` homogeneous `PickingItem` resources with one `parameters` bag and the same 20 parameter definitions per resource. The generated values are mostly identical so the report can be used as the baseline for later value canonicalization, shared schema, and sparse override prototypes.

Useful system properties:

- `parameterMemoryFootprintBaseline.resources`: number of synthetic resources, default `100000`.
- `parameterMemoryFootprintBaseline.operations`: operation count for each lookup and mutation micro-scenario, default `50000`.

The report includes:

- Retained heap total, MiB, bytes per resource, and bytes per parameter using JOL `GraphLayout`.
- Object counts and retained heap by class, including parameters, parameter bags, maps, map nodes, boxed values, strings, dates, and lists.
- Simple timing scenarios for `getParameter(...)`, `getString(...)`, `setValue(...)`, `clear()`, and `getClone()`.

Keep the test opt-in for normal builds because the full baseline intentionally allocates a large object graph.