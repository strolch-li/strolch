
DROP TABLE IF EXISTS resources;
DROP TABLE IF EXISTS orders;
DROP TABLE IF EXISTS activities;

DROP TABLE IF EXISTS audits;

DROP TABLE IF EXISTS operations_log;
DROP TABLE IF EXISTS operations_log_values;

DROP TABLE IF EXISTS db_version;

DROP TYPE IF EXISTS order_state;
DROP TYPE IF EXISTS access_type;
DROP TYPE IF EXISTS log_severity_type;
DROP TYPE IF EXISTS log_state_type;

DROP INDEX IF EXISTS ids_orders_date;
