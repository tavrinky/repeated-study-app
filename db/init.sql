

CREATE TABLE IF NOT EXISTS users(
    id SERIAL PRIMARY KEY, 
    username TEXT NOT NULL, 
    password BYTEA NOT NULL, 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp() 
); 

CREATE TABLE IF NOT EXISTS card(
    id SERIAL PRIMARY KEY, 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp(), 
    deleted_at TIMESTAMPTZ, 
    data JSONB NOT NULL, 
    created_by INT NOT NULL, 
    CONSTRAINT fk_card FOREIGN KEY(created_by) REFERENCES users(id) ON DELETE CASCADE
); 
