# Reporting Framework

The Strolch Reporting Framework allows for the definition and execution of complex queries against the Strolch model, producing flat or hierarchical data suitable for display or export.

## Core Concepts

### Report

The `Report` class is the main entry point for executing a report. It is initialized with a `reportId`, which refers to a `Resource` of type `Report` in the Strolch model.

```java
try (Report report = new Report(tx, "my_report_id")) {
    // Optional: add programmatically filters
    report.filter("Product", "product_1", "product_2");
    
    // Optional: add date range
    report.dateRange(new DateRange().from(start, true).to(end, true));

    // Execute report
    Stream<ReportElement> result = report.doReport();
    
    // Process as JSON
    Stream<JsonObject> jsonResult = report.doReportAsJson();
}
```

### Java API

- `doReport()`: Returns a `Stream<ReportElement>` containing the report data.
- `doReportWithPage(offset, limit)`: Executes the report and returns a paginated stream.
- `doReportAsJson()`: Returns the results as a stream of GSON `JsonObject`s.
- `generateFilterCriteria(limit)`: Returns a map of possible values for each filterable column, useful for building UI facets.
- `filter(type, ids)`: Programmatically adds a filter for a specific element type and a set of IDs.
- `dateRange(dateRange)`: Sets the date range for the report (if `dateRangeSel` is configured).

### Report Resource

A report is defined as a `Resource` in the model with the type `Report`. It contains multiple `ParameterBag`s to configure the report's behavior.

- **`parameters` Bag**: General configuration (object type, parallel execution, ordering).
- **`columns` Bag**: Defines the output columns and where to retrieve their values.
- **`joins` Bag**: Defines relationships between different element types.
- **`Filter` Bags**: (ParameterBags of type `Filter`) Define criteria for including elements.
- **`ordering` Bag**: Defines the default sort order.
- **`formattingHints` Bag**: Specifies how values should be formatted (e.g., dates).
- **`Policy`**: Usually set to `li.strolch.report.policy.GenericReport`.

## GenericReport Configuration

`GenericReport` is the default, configuration-driven policy. It builds a report by querying a base element type and optionally joining it with related elements.

### Base Elements

The root element type is defined in the `parameters` bag using the `objectType` parameter.

```xml
<Parameter Id="objectType" Name="Object Type" Type="String" Interpretation="Order-Ref" Uom="PurchaseOrder" Value="PurchaseOrder"/>
```

Common configuration parameters in the `parameters` bag:
- `parallel`: (Boolean) Enable parallel processing of the elements.
- `descending`: (Boolean) Default sort order (applies to the first ordering criteria).
- `allowMissingColumns`: (Boolean) If true, rows with missing parameters are still included in the report.
- `dateRangeSel`: (String) Reference to a date parameter used for filtering by date range.
- `filterMissingValuesAsTrue`: (Boolean) If true, filters will treat missing values as matching (useful for optional parameters).
- `maxRowsForFacetGeneration`: (Integer) Limit the number of rows processed to generate filter criteria (facets).
- `maxFacetValues`: (Integer) Limit the number of discrete values returned per facet.
- `directCriteria`: (StringList) A list of element types for which filter criteria should be queried directly from the model (performance optimization).

### Columns

Columns define the data returned for each row. The `Id` is the column identifier, `Name` is the display name, and `Value` specifies how to retrieve the data.

```xml
<ParameterBag Id="columns" Name="Display Columns" Type="Display">
  <Parameter Id="id" Name="ID" Type="String" Value="$id" Index="10"/>
  <Parameter Id="name" Name="Name" Type="String" Value="$name" Index="20"/>
  <Parameter Id="state" Name="Status" Type="String" Value="$state" Index="30"/>
  <Parameter Id="created" Name="Created Date" Type="String" Value="$date" Index="40"/>
  <Parameter Id="price" Name="Price" Type="String" Value="Bags/parameters/price" Index="50"/>
  <Parameter Id="customerName" Name="Customer" Type="String" Interpretation="Resource-Ref" Uom="Customer" Value="$name" Index="60"/>
</ParameterBag>
```

**Special Values:**
- `$id`: The element's ID.
- `$name`: The element's name.
- `$type`: The element's type.
- `$state`: The element's state (for Orders and Activities).
- `$date`: The element's date (for Orders and Activities).
- `Bags/bagId/paramId`: Value of a parameter.

### Joins

Joins link the root element to other elements in the model.

```xml
<ParameterBag Id="joins" Name="Joins" Type="Joins">
  <Parameter Id="Customer" Name="Customer" Type="String" Interpretation="Resource-Ref" Uom="Customer" Value="PurchaseOrder"/>
  <Parameter Id="Location" Name="Location" Type="String" Interpretation="Resource-Ref" Uom="Location" Value="Customer"/>
</ParameterBag>
```
- `Id`: The joined element's type (alias).
- `Uom`: The actual type in the Strolch model.
- `Value`: The element type to join from (must be the root type or a previously joined type).

### Filtering

Filters exclude elements based on their parameter values. They are defined as `ParameterBag`s with `Type="Filter"`.

```xml
<ParameterBag Id="activeFilter" Name="Active Filter" Type="Filter">
  <Parameter Id="fieldRef" Name="Field Reference" Type="String" Value="Bags/parameters/status"/>
  <Parameter Id="policy" Name="Filter Policy" Type="String" Interpretation="ReportFilterPolicy" Uom="key:Equals" Value="active"/>
</ParameterBag>
```
- `fieldRef`: The path to the parameter to check.
- `policy`: The filter logic (Interpretation `ReportFilterPolicy`).
  - `key:Equals`: Exact match.
  - `key:Contains`: Substring match.
  - `key:IsIn`: Value is in a comma-separated list.
  - `key:GreaterThan` / `key:LessThan`: Comparison.
  - `key:IsEmpty`: Check for null or empty.
- `Value`: The comparison value. Supports dynamic values like `now()`, `now(-P1D)` (yesterday), `now(-P1M)` (one month ago).

### Ordering

Defines how the results are sorted.

```xml
<ParameterBag Id="ordering" Name="Ordering" Type="Ordering">
  <Parameter Id="date" Name="Date" Type="String" Value="$date" Index="10"/>
  <Parameter Id="name" Name="Name" Type="String" Value="$name" Index="20"/>
</ParameterBag>
```

## Advanced Examples

### Example 1: User Audit Report
Reporting on `User` resources, showing basic properties and a custom "Role" parameter.

```xml
<Resource Id="userReport" Name="User Activity Report" Type="Report">
  <ParameterBag Id="parameters" Name="Parameters">
    <Parameter Id="objectType" Type="String" Interpretation="Resource-Ref" Uom="User" Value="User"/>
  </ParameterBag>
  <ParameterBag Id="columns" Name="Columns" Type="Display">
    <Parameter Id="username" Name="Username" Type="String" Value="$id" Index="10"/>
    <Parameter Id="fullname" Name="Full Name" Type="String" Value="$name" Index="20"/>
    <Parameter Id="role" Name="Role" Type="String" Value="Bags/parameters/role" Index="30"/>
    <Parameter Id="lastLogin" Name="Last Login" Type="String" Value="Bags/parameters/lastLogin" Index="40"/>
  </ParameterBag>
  <ParameterBag Id="formattingHints" Name="Formatting" Type="FormattingHint">
    <Parameter Id="lastLogin" Type="String" Value="DateTime"/>
  </ParameterBag>
  <Policies>
    <Policy Type="ReportPolicy" Value="li.strolch.report.policy.GenericReport"/>
  </Policies>
</Resource>
```

### Example 2: Inventory Hierarchy Report
Joining `Slot` -> `Section` -> `Storage` -> `Location`.

```xml
<Resource Id="inventoryReport" Name="Detailed Inventory Report" Type="Report">
  <ParameterBag Id="parameters" Name="Parameters">
    <Parameter Id="objectType" Type="String" Interpretation="Resource-Ref" Uom="Slot" Value="Slot"/>
    <Parameter Id="parallel" Type="Boolean" Value="true"/>
  </ParameterBag>
  <ParameterBag Id="joins" Name="Joins" Type="Joins">
    <Parameter Id="Section" Type="String" Interpretation="Resource-Ref" Uom="Section" Value="Slot"/>
    <Parameter Id="Storage" Type="String" Interpretation="Resource-Ref" Uom="Storage" Value="Section"/>
    <Parameter Id="Location" Type="String" Interpretation="Resource-Ref" Uom="Location" Value="Storage"/>
  </ParameterBag>
  <ParameterBag Id="columns" Name="Columns" Type="Display">
    <Parameter Id="location" Name="Facility" Type="String" Interpretation="Resource-Ref" Uom="Location" Value="$name" Index="10"/>
    <Parameter Id="storage" Name="Warehouse" Type="String" Interpretation="Resource-Ref" Uom="Storage" Value="$name" Index="20"/>
    <Parameter Id="section" Name="Aisle" Type="String" Interpretation="Resource-Ref" Uom="Section" Value="$name" Index="30"/>
    <Parameter Id="slot" Name="Bin" Type="String" Value="$name" Index="40"/>
    <Parameter Id="qty" Name="Quantity" Type="String" Value="Bags/parameters/quantity" Index="50"/>
  </ParameterBag>
  <Policies>
    <Policy Type="ReportPolicy" Value="li.strolch.report.policy.GenericReport"/>
  </Policies>
</Resource>
```

### Example 3: Expiring Products Report
Using filters with dynamic date functions.

```xml
<Resource Id="expiringProducts" Name="Expiring Products" Type="Report">
  <ParameterBag Id="parameters" Name="Parameters">
    <Parameter Id="objectType" Type="String" Interpretation="Resource-Ref" Uom="Batch" Value="Batch"/>
    <Parameter Id="dateRangeSel" Type="String" Value="Bags/parameters/expiryDate"/>
  </ParameterBag>
  <ParameterBag Id="expiringFilter" Name="Filter" Type="Filter">
    <Parameter Id="fieldRef" Type="String" Value="Bags/parameters/expiryDate"/>
    <Parameter Id="policy" Type="String" Interpretation="ReportFilterPolicy" Uom="key:LessThan" Value="now(+P3M)"/>
  </ParameterBag>
  <ParameterBag Id="columns" Name="Columns" Type="Display">
    <Parameter Id="batchId" Name="Batch ID" Type="String" Value="$id" Index="10"/>
    <Parameter Id="product" Name="Product" Type="String" Interpretation="Resource-Ref" Uom="Product" Value="$name" Index="20"/>
    <Parameter Id="expiryDate" Name="Expiry Date" Type="String" Value="Bags/parameters/expiryDate" Index="30"/>
  </ParameterBag>
  <ParameterBag Id="formattingHints" Name="Formatting" Type="FormattingHint">
    <Parameter Id="expiryDate" Type="String" Value="Date"/>
  </ParameterBag>
  <Policies>
    <Policy Type="ReportPolicy" Value="li.strolch.report.policy.GenericReport"/>
  </Policies>
</Resource>
```

### Example 4: Additional Type Join
Reporting on `Orders` and joining with `Products` via a specific parameter, even if there is no direct Strolch Relation.

```xml
<Resource Id="orderProductReport" Name="Order Product Details" Type="Report">
  <ParameterBag Id="parameters" Name="Parameters">
    <Parameter Id="objectType" Type="String" Interpretation="Order-Ref" Uom="SalesOrder" Value="SalesOrder"/>
  </ParameterBag>
  <ParameterBag Id="additionalType" Name="Additional Type" Type="AdditionalType">
    <Parameter Id="objectType" Type="String" Interpretation="Resource-Ref" Uom="Product" Value="Product"/>
    <Parameter Id="joinParam" Name="Join Param" Type="String" Value="Bags/parameters/externalId"/>
    <Parameter Id="joinWith" Name="Join With" Type="String" Interpretation="Order-Ref" Uom="SalesOrder" Value="Bags/parameters/productId"/>
  </ParameterBag>
  <ParameterBag Id="columns" Name="Columns" Type="Display">
    <Parameter Id="orderId" Name="Order" Type="String" Value="$id" Index="10"/>
    <Parameter Id="productName" Name="Product" Type="String" Interpretation="Resource-Ref" Uom="Product" Value="$name" Index="20"/>
    <Parameter Id="category" Name="Category" Type="String" Interpretation="Resource-Ref" Uom="Product" Value="Bags/parameters/category" Index="30"/>
  </ParameterBag>
  <Policies>
    <Policy Type="ReportPolicy" Value="li.strolch.report.policy.GenericReport"/>
  </Policies>
</Resource>
```

## Usage in Services

Reports are often used within services to provide data for the frontend or for background exports.

```java
public class ExportReportService extends AbstractService<StringArgument, StringResult> {
    @Override
    protected StringResult internalDoService(StringArgument arg) throws Exception {
        try (StrolchTransaction tx = openArgOrUserTx(arg);
             Report report = new Report(tx, arg.value)) {
            
            // Generate CSV from report stream
            StringBuilder sb = new StringBuilder();
            
            // Header
            sb.append(String.join(",", report.getColumnKeys())).append("\n");
            
            // Rows
            report.doReport().forEach(el -> {
                String row = report.getColumnKeys().stream()
                    .map(el::getColumn)
                    .collect(Collectors.joining(","));
                sb.append(row).append("\n");
            });
            
            return new StringResult(sb.toString());
        }
    }
}
```
