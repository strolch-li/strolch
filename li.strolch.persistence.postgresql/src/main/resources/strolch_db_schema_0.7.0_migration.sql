
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'log_severity_type') THEN
        CREATE TYPE log_severity_type AS ENUM ('Info', 'Notification', 'Warning', 'Error', 'Exception');
    END IF;
END$$;


CREATE TABLE IF NOT EXISTS operations_log (
  id varchar(255) PRIMARY KEY,
	realm varchar(255),
	dateTime timestamp with time zone,
	username varchar(255),
	severity log_severity_type,
	locator varchar(1024),
	key varchar(255),
	message text,
	stacktrace text
);

CREATE TABLE IF NOT EXISTS operations_log_values (
  id varchar(255),
  key varchar(255),
  value text
);


INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.7.0',
  'strolch',
  'Added persisting of operations log',
  CURRENT_TIMESTAMP
);
