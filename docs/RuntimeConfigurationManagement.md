# Configuration Management Specification

### Overview & Goals
The Strolch framework manages its runtime configuration through a specialized `Resource` (accessible via `tx.getConfiguration()`) and its policy mappings through a `PolicyHandler` (initialized from `StrolchPolicies.xml`).

This feature provides:
- A unified `ConfigurationPolicy` to access and modify these configurations.
- REST API endpoints for remote management.
- Runtime updates to policy mappings with XML persistence.
- Automatic discovery of policy implementations via classpath scanning.

### Security
Access to the configuration API is protected by the `StrolchConfiguration` privilege.

#### Required Privilege Configuration
To allow a user to manage the configuration, the following privilege must be added to their role:

```xml
<Privilege name="StrolchConfiguration" policy="DefaultPrivilege">
	<Allow>GetConfiguration</Allow>
	<Allow>UpdateConfiguration</Allow>
	<Allow>GetPolicyModel</Allow>
	<Allow>UpdatePolicyModel</Allow>
</Privilege>
```

Alternatively, to allow all actions:

```xml
<Privilege name="StrolchConfiguration" policy="DefaultPrivilege">
	<AllAllowed>true</AllAllowed>
</Privilege>
```

### REST API

The API is available at the base path `/strolch/configuration`.

#### 1. Configuration Resource
Manages the core Strolch configuration (the "configuration" Resource).

- **GET `/resource`**
  - **Action**: `GetConfiguration`
  - **Returns**: The configuration `Resource` as JSON.
- **PUT `/resource`**
  - **Action**: `UpdateConfiguration`
  - **Body**: The updated `Resource` as JSON.
  - **Description**: Updates the configuration in the system and triggers any necessary component reloads.

#### 2. Policy Model
Manages the mappings of policy types to implementations.

- **GET `/policies`**
  - **Action**: `GetPolicyModel`
  - **Returns**: A JSON object containing all registered policy types, their current implementations, and all possible implementations found on the classpath.
- **PUT `/policies`**
  - **Action**: `UpdatePolicyModel`
  - **Body**: The updated `PolicyModel` as JSON.
  - **Description**: Updates the internal policy mappings and persists them back to `StrolchPolicies.xml`.

### Technical Details

#### ConfigurationPolicy
- **Interface**: `li.strolch.policy.ConfigurationPolicy`
- **Default Implementation**: `li.strolch.policy.DefaultConfigurationPolicy`

#### Policy Discovery
The system automatically scans the classpath for classes extending `li.strolch.policy.StrolchPolicy`. The results are cached to ensure performance.

#### XML Persistence
Updates to policies are saved back to the `StrolchPolicies.xml` file, ensuring that changes survive a system restart.

### JSON Examples

**Configuration Resource (GET /resource)**
*Note: The response is wrapped in a `data` field.*
```json
{
  "msg": "OK",
  "status": "OK",
  "data": {
    "objectType": "Resource",
    "id": "configuration",
    "name": "Strolch Configuration",
    "type": "Configuration",
    "parameterBags": {
      "parameters": {
        "id": "parameters",
        "name": "Parameters",
        "type": "Parameters",
        "parameters": {
          "environment": {
            "id": "environment",
            "name": "Environment",
            "type": "String",
            "value": "dev"
          }
        }
      }
    }
  }
}
```

**Configuration Resource Update (PUT /resource)**
*Note: Send the resource JSON directly as the request body.*
```json
{
  "objectType": "Resource",
  "id": "configuration",
  "name": "Updated Strolch Configuration",
  "type": "Configuration",
  "parameterBags": {
    "parameters": {
      "id": "parameters",
      "name": "Parameters",
      "type": "Parameters",
      "parameters": {
        "environment": {
          "id": "environment",
          "name": "Environment",
          "type": "String",
          "value": "production"
        }
      }
    }
  }
}
```

**Policy Model (GET /policies)**
*Note: The response is wrapped in a `data` field.*
```json
{
  "msg": "OK",
  "status": "OK",
  "data": {
    "policyTypes": {
      "ConfigurationPolicy": {
        "type": "ConfigurationPolicy",
        "api": "li.strolch.policy.ConfigurationPolicy",
        "policyByKeyMap": {
          "DefaultConfigurationPolicy": "li.strolch.policy.DefaultConfigurationPolicy"
        },
        "possibleImplementations": [
          "li.strolch.policy.DefaultConfigurationPolicy",
          "li.strolch.policy.AlternativeConfigurationPolicy"
        ]
      }
    }
  }
}
```

**Policy Model Update (PUT /policies)**
*Note: Send the policy model JSON directly as the request body.*
```json
{
  "policyTypes": {
    "ConfigurationPolicy": {
      "type": "ConfigurationPolicy",
      "api": "li.strolch.policy.ConfigurationPolicy",
      "policyByKeyMap": {
        "DefaultConfigurationPolicy": "li.strolch.policy.AlternativeConfigurationPolicy"
      }
    }
  }
}
```
