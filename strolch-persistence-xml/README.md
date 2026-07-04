# strolch-persistence-xml

XML Persistence Implementation for Strolch.

## Overview
This module provides a filesystem-based XML persistence implementation for the Strolch framework. It stores Strolch elements (Resources, Orders, Activities, Audits, and LogMessages) as individual XML files on the local disk. It is ideal for small projects, development environments, or as a reference implementation.

## Features
- Full support for core Strolch element types.
- Filesystem-based storage (no database server required).
- Multiple realm support.
- Automatic data initialization on empty storage.
- High performance SAX parsing.

## Documentation
- [Technical Specification](docs/technical-spec.md) - Detailed architecture and configuration guide.

## Setup

### 1. Strolch Configuration
Configure the `PersistenceHandler` in your `StrolchConfiguration.xml`:

```xml
<Component>
    <name>PersistenceHandler</name>
    <api>li.strolch.persistence.api.StrolchPersistenceHandler</api>
    <impl>li.strolch.persistence.xml.XmlPersistenceHandler</impl>
    <Properties>
        <dbStorePath>dbStore</dbStorePath>
        <allowDataInitOnEmptyDb>true</allowDataInitOnEmptyDb>
    </Properties>
</Component>
```

### 2. Data Initialization
If `allowDataInitOnEmptyDb` is set to `true`, Strolch will automatically populate the storage from the realm's initial data file if the `dbStorePath` directory is empty.

## Development and Testing

To run the tests for this module:
```bash
mvn test
```

## References
- [Strolch Framework](https://strolch.li)
- [strolch-xmlpers](../strolch-xmlpers)
