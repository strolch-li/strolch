# strolch-service

The `strolch-service` module provides the Service API for the Strolch framework. It encapsulates business logic into reusable Services and Commands, and provides specialized frameworks for execution, reporting, and migrations.

## Overview

Strolch business logic is organized into several key areas within this module:

*   **Services**: The high-level entry points for business logic, providing transaction management and privilege checking.
*   **Commands**: Atomic, reusable operations performed within a transaction.
*   **Execution Framework**: A powerful engine for planning and executing complex `Activity` hierarchies.
*   **Reporting Framework**: A flexible system for generating reports and data exports from the Strolch model.
*   **Migrations**: A structured way to handle model and code changes across versions.

## Key Packages

*   `li.strolch.service`: Contains base service classes (`AbstractService`) and many generic CRUD services.
*   `li.strolch.command`: Provides atomic commands for manipulating Resources, Orders, and Activities.
*   `li.strolch.execution`: Implements the `ExecutionHandler` and related policies for activity execution.
*   `li.strolch.report`: Provides the `Report` class and policies for generating reports.
*   `li.strolch.migrations`: Handles data and code migrations during system startup.

## Documentation

Detailed technical documentation can be found in the `docs/` directory:

*   [Services](docs/services.md)
*   [Execution Framework](docs/execution.md)
*   [Reporting Framework](docs/reporting.md)
*   [Migrations](docs/migrations.md)

## Running tests

Start PostgreSQL console and create the users:

	$ sudo -u postgres psql
	$ postgres=# 
	create user cacheduser with password 'test';
	create database cacheduserdb owner cacheduser;
	GRANT CONNECT ON DATABASE cacheduserdb TO cacheduser;
	
	create user cacheduserauditsversioning with password 'test';
	create database cacheduserauditsversioningdb owner cacheduserauditsversioning;
	GRANT CONNECT ON DATABASE cacheduserauditsversioningdb TO cacheduserauditsversioning;

You can revoke the privileges with the following:

	revoke ALL PRIVILEGES ON DATABASE cacheduserdb from cacheduser;
	drop user cacheduser;
	drop database cacheduserdb;
	
	revoke ALL PRIVILEGES ON DATABASE transactionaluserdb from transactionaluser;
	drop user transactionaluser;
	drop database transactionaluserdb;
	
	revoke ALL PRIVILEGES ON DATABASE cacheduserauditsversioningdb from cacheduserauditsversioning;
	drop user cacheduserauditsversioning;
	drop database cacheduserauditsversioningdb;
