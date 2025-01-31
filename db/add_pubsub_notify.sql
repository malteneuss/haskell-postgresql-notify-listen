CREATE TABLE task (
    id         Serial PRIMARY KEY, 
    payload    VARCHAR NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE OR REPLACE FUNCTION notify_task()
RETURNS trigger AS
$$
BEGIN
    -- PERFORM pg_notify('task_listener', row_to_json(NEW)::text);
    PERFORM pg_notify('task_listener', 'Empty message');
    RETURN NEW;
END;
$$
LANGUAGE plpgsql;

CREATE TRIGGER trigger_notify_task
    AFTER INSERT ON task
    FOR EACH ROW EXECUTE FUNCTION notify_task();


