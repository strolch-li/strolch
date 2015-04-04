
-- DB_VERSION
ALTER TABLE audits ADD COLUMN element_sub_type character varying(255) NOT NULL DEFAULT '-';

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.3.0',
  'strolch',
  'Added new column element_sub_type to table audits',
  CURRENT_TIMESTAMP
);
