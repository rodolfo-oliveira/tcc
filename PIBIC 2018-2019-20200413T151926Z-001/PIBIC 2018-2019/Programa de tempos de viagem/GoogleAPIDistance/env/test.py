import csv
import random
import localfunctions
import numpy
import googlemaps
import json
import datetime
import mysql_sample 


dbasename= 'aaaa'

query = """INSERT INTO {} (year, hour, weekday, origincoord, originadress, destincoord, destinadress, duration, distance, fare) 
			VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)""".format(dbasename)

print query