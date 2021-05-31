#!/bin/bash

echo "=========== Launching Oracle ==========="
echo -e "sga_target=1536M" >> /opt/oracle/product/18c/dbhomeXE/dbs/initXE.ora; /opt/oracle/product/18c/dbhomeXE/bin/lsnrctl start ; echo create spfile from pfile\; | /opt/oracle/product/18c/dbhomeXE/bin/sqlplus / as sysdba ;echo startup | /opt/oracle/product/18c/dbhomeXE/bin/sqlplus / as sysdba ; /opt/oracle/product/18c/dbhomeXE/bin/lsnrctl status ;

echo "=========== Setting Up Test Oracle Database Schema ==========="
/opt/oracle/product/18c/dbhomeXE/bin/sqlplus quill_test/QuillRocks! < /quill_setup/oracle-schema.sql

echo "=========== Oracle Schema Setup Complete ==========="
tail -f /opt/oracle/diag/rdbms/*/*/trace/alert*.log
