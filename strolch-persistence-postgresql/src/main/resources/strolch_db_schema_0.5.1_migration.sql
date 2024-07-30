
CREATE TYPE order_state1 AS ENUM ('CREATED', 'PLANNING', 'PLANNED', 'EXECUTION', 'STOPPED', 'WARNING', 'ERROR', 'EXECUTED', 'CLOSED');

-- Convert to new type, casting via text representation
ALTER TABLE orders 
  ALTER COLUMN state TYPE order_state1 
    USING (state::text::order_state1);

DROP TYPE order_state;
ALTER TYPE order_state1 RENAME TO order_state;

-- add state columns
ALTER TABLE activities ADD COLUMN state order_state;

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.5.1',
  'strolch',
  'Added state column to activity, and added new states',
  CURRENT_TIMESTAMP
);
