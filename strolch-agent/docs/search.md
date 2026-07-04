### Strolch Search API

The Search API provides a fluent and powerful way to query the Strolch data model. It supports filtering by type, attributes, and complex expressions.

#### Search Classes

-   **`ResourceSearch`**: For querying Resources.
-   **`OrderSearch`**: For querying Orders.
-   **`ActivitySearch`**: For querying Activities.

#### Basic Usage

A search is typically executed within a transaction.

```java
List<Resource> results = new ResourceSearch()
    .types("Product")
    .where(id(isEqualTo("p01")))
    .search(tx)
    .toList();
```

#### Filtering

Filters are added using the `where` method and `SearchExpression`s. Strolch provides many built-in predicates.

-   **ID and Name**: `id(isEqualTo("..."))`, `name(contains("..."))`
-   **Parameters**: `param("bagId", "paramId", isEqualTo("value"))`
-   **Date Ranges**: `date(isEqualTo(...))`, `date(isBefore(...))`, `date(isAfter(...))`

#### Predicates

Strolch supports various predicates for comparison:
-   `isEqualTo(value)`
-   `isNotEqualTo(value)`
-   `contains(value)`
-   `startsWith(value)`
-   `endsWith(value)`
-   `isIn(collection)`
-   `isEmpty()`

#### Complex Expressions

Expressions can be combined using logical operators.

```java
new ResourceSearch()
    .types("Person")
    .where(param("parameters", "firstName", isEqualTo("John"))
        .and(param("parameters", "lastName", isEqualTo("Doe"))))
    .search(tx)
    .toList();
```

#### Navigation

You can navigate through relations between elements. Strolch provides specific expressions for this:

-   **`relationName`**: Filter by the name of a related element.
-   **`relationParam`**: Filter by a parameter of a related element.
-   **`relationNull`**: Filter for elements where a relation is missing.

Note that `relationName` and `relationParam` require the current transaction `tx` to look up the related element.

```java
new ResourceSearch()
    .types("Slot")
    .where(relationName(tx, "location", isEqualTo("Warehouse-A")))
    .search(tx)
    .toList();
```

To filter by a parameter on a related element:

```java
new ResourceSearch()
    .types("Slot")
    .where(relationParam(tx, "location", "parameters", "color", isEqualTo("yellow")))
    .search(tx)
    .toList();
```

If the parameter is in the default `parameters` bag, you can use the shorthand:

```java
new ResourceSearch()
    .types("Slot")
    .where(relationParam(tx, "location", "color", isEqualTo("yellow")))
    .search(tx)
    .toList();
```

To find elements where a relation is missing:

```java
new ResourceSearch()
    .types("Slot")
    .where(relationNull("location"))
    .search(tx)
    .toList();
```

#### Working with Search Results

The `search(tx)` method returns a `SearchResult` (or `RootElementSearchResult` for core elements), which provides a fluent API for processing the results.

##### Terminal Operations

Terminal operations execute the search and return a result.

-   **`toList()` / `toSet()`**: Collect results into a `List` or `Set`.
-   **`toMap()`**: Collect results into a `Map`.
-   **`toMapOfLists()` / `toMapOfSets()` / `toMapOfMaps()`**: Collect results into specialized multi-maps.
-   **`toJsonArray(jsonMapper)`**: Convert results to a `JsonArray`.
-   **`toPaging(offset, limit)`**: Return a `Paging` object for paginated results.
-   **`toSingleton()`**: Expect exactly one result, throws an exception otherwise.
-   **`toSingletonO()`**: Returns an `Optional` containing the single result, or empty if none. Throws if more than one.
-   **`isEmpty()` / `isNotEmpty()`**: Check if any results match.
-   **`forEach(consumer)`**: Perform an action for each result.

##### Transformations and Filtering

-   **`map(mapper)`**: Transform the elements in the result stream.
-   **`filter(predicate)`**: Appends a filter to the result stream.
-   **`visitor(visitor)`**: Specific to `RootElementSearchResult`, transforms elements using a `StrolchRootElementVisitor`.
-   **`asStream()`**: Access the underlying Java `Stream<T>` for custom processing.

##### Ordering

`RootElementSearchResult` provides several convenience methods for sorting:

-   **`orderById(reversed)`**: Sort by element ID.
-   **`orderByName(reversed)`**: Sort by element name.
-   **`orderByParam(bagId, paramId, reversed)`**: Sort by a specific parameter value.
-   **`orderBy(comparator)`**: Sort using a custom `Comparator`.

##### Concurrency and Modifiability

When working with Strolch elements, you might need to modify them:

-   **`cloneIfReadOnly()`**: Clones any read-only elements in the stream. Use this if you intend to modify the elements.
-   **`readLock(tx)`**: Performs a `readLock` on each element in the stream to ensure exclusive access within the transaction.

#### Modalities

-   **In-Memory**: Searches are performed against the elements in the current transaction and the underlying persistence layer.
-   **Caching**: If the transaction contains cached elements, these elements will be used, not the elements from the underlying persistence layer. This is important as cached elements might have been modified in the current transaction.
-   **Streaming**: The `search(tx)` method returns a `SearchResult` which allows streaming the results for efficient processing of large data sets.
