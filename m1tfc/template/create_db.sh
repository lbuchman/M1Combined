#!/bin/sh

check_return_code()
{
   rc=$(expr $rc + $1)
}

DB_REV=1
SQLITE_COMMAND=sqlite3
DB=template.db
ABSDIR=$(dirname $(readlink -f $0))
UUID=`uuidgen`

rc=0

rm -f $DB

echo $SQLITE_COMMAND
echo $DB
echo $ABSDIR

echo '+Create Database Script for boards Begin'

echo ' -DBREV'
# Create database for firmware
# UUID is being added to track database state

#$SQLITE_COMMAND $DB "CREATE TABLE DBREV ( rev integer PRIMARY KEY AUTOINCREMENT, updatetime varchar, uuid varchar)" || check_return_code $?
#$SQLITE_COMMAND $DB "insert into DBREV values ($DB_REV,datetime('NOW'),'$UUID')" || check_return_code $?

###records###
echo ' -records' $ABSDIR/dbcreateScript
$SQLITE_COMMAND $DB < $ABSDIR/dbcreateScript || check_return_code $?
#$SQLITE_COMMAND $DB "insert into records values ('00:0F:A6:00:00:02',datetime('NOW'),'NA','NA','NA','',false,false,false)" || check_return_code $?
#$SQLITE_COMMAND $DB "insert into uid values ('00:0F:A6:00:00:02')" || check_return_code $?
$SQLITE_COMMAND $DB "insert into perm values (1, 1)" || check_return_code $?



echo '+Create DB script end'

exit $rc
