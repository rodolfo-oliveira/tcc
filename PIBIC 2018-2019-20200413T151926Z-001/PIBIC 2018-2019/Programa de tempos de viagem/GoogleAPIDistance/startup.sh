#! /bin/bash

sudo apt-get update


./cloud_sql_proxy -instances=indicador-de-mobilidade:us-east1:indicador-de-mobilidade2 -dir=/cloudsql &

python program/main.py