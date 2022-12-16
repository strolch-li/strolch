
-- ACTIVITIES
CREATE TABLE IF NOT EXISTS activities (
  id varchar(255) PRIMARY KEY, 
  name VARCHAR(255),
  type VARCHAR(255),
  asxml xml
);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.4.0',
  'strolch',
  'Added new table activities',
  CURRENT_TIMESTAMP
);
