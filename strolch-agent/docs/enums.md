### Enum Handler

The `EnumHandler` provides a way to manage and query enumerated values within Strolch. These are typically used for providing lists of values in a UI (e.g., dropdowns) and for internationalization.

#### Concepts

-   **`StrolchEnum`**: Represents a collection of key-value pairs for a specific locale.
-   **`EnumHandler`**: Component for retrieving enums from the data model.

#### Defining Enums

Enums are defined as `Resource` elements of type `Enum` in the Strolch model. Each `ParameterBag` represents a locale, and the `Parameter`s within that bag represent the enum values.

```xml
<Resource Id="sex" Name="Sex" Type="Enum">
    <ParameterBag Id="en" Name="English">
        <Parameter Id="male" Name="Male" Type="String" Value="Male"/>
        <Parameter Id="female" Name="Female" Type="String" Value="Female"/>
    </ParameterBag>
    <ParameterBag Id="de" Name="German">
        <Parameter Id="male" Name="Männlich" Type="String" Value="Männlich"/>
        <Parameter Id="female" Name="Weiblich" Type="String" Value="Weiblich"/>
    </ParameterBag>
</Resource>
```

#### Retrieving Enums

You can retrieve an enum through the `EnumHandler` using a `Certificate` and a `Locale`.

```java
EnumHandler enumHandler = agent.getComponent(EnumHandler.class);
StrolchEnum sexEnum = enumHandler.getEnum(certificate, "sex", Locale.ENGLISH);

String maleLabel = sexEnum.getValue("male"); // Returns "Male"
```

#### Retrieving Enums in Transactions

Within a transaction, the `EnumHandler` can use the transaction's context to determine the locale.

```java
try (StrolchTransaction tx = agent.openTx(certificate, "GetEnum", true)) {
    EnumHandler enumHandler = tx.getComponent(EnumHandler.class);
    StrolchEnum sexEnum = enumHandler.getEnum(tx, "sex");
    // ...
}
```

#### Key Features

-   **Internationalization**: Easily manage translations for enum values.
-   **Dynamic Values**: Enums are stored in the data model, so they can be modified without changing code.
-   **Integration**: Seamlessly integrated with the Strolch data model and transaction system.
