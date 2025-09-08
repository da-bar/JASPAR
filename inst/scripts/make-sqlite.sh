#!/bin/bash

wget https://jaspar.elixir.no/download/database/JASPAR2024.sql.gz
gunzip -c JASPAR2024.sql.gz | sqlite3 JASPAR2024.sqlite

wget https://jaspar2022.genereg.net/download/database/JASPAR2022.sql.gz
gunzip -c JASPAR2022.sql.gz | sqlite3 JASPAR2022.sqlite
