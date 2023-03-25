

CREATE TABLE IF NOT EXISTS student_user(
    id SERIAL PRIMARY KEY, 
    username TEXT NOT NULL, 
    password BYTEA NOT NULL, 
    salt BYTEA NOT NULL, 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp() 
); 

CREATE TABLE IF NOT EXISTS organization_user(
    id SERIAL PRIMARY KEY, 
    username TEXT NOT NULL UNIQUE, 
    password BYTEA NOT NULL, 
    salt BYTEA NOT NULL, 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp() 
);

CREATE TABLE IF NOT EXISTS organization(
    id SERIAL PRIMARY KEY, 
    org_name TEXT NOT NULL, 
    created_by INT NOT NULL REFERENCES organization_user(id), 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp() 
);

CREATE TABLE IF NOT EXISTS teacher_user(
    id SERIAL PRIMARY KEY, 
    username TEXT NOT NULL, 
    password BYTEA NOT NULL, 
    salt BYTEA NOT NULL,  
    created_at TIMESTAMPTZ DEFAULT clock_timestamp(), 
    organization INT NOT NULL REFERENCES organization(id) 
); 

CREATE TABLE IF NOT EXISTS card(
    id SERIAL PRIMARY KEY, 
    data JSONB NOT NULL, 
    created_by INT NOT NULL REFERENCES teacher_user(id), 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp()
); 

CREATE TABLE IF NOT EXISTS deck( 
    id SERIAL PRIMARY KEY, 
    data JSONB NOT NULL, 
    created_by INT NOT NULL REFERENCES teacher_user(id), 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp() 
);

CREATE TABLE IF NOT EXISTS deck_card(
    id SERIAL PRIMARY KEY, 
    card_id INT NOT NULL REFERENCES card(id),  
    deck_id INT NOT NULL REFERENCES deck(id) 
);

CREATE TABLE IF NOT EXISTS card_study(
    id SERIAL PRIMARY KEY, 
    card_id INT NOT NULL REFERENCES card(id), 
    student INT NOT NULL REFERENCES student_user(id), 
    succeeds BOOLEAN NOT NULL, 
    created_at TIMESTAMPTZ DEFAULT clock_timestamp() 
); 
