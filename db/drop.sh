#!/usr/bin/bash

PGPASSWORD='password' psql -U ankidb -h 127.0.0.1 -d ankidb -f ./drop.sql;
unset PGPASSWORD;