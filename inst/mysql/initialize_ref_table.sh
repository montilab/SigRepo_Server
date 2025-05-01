#!/bin/bash

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/schema/init.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/keywords.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/organisms.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/phenotypes.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/platforms.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/sample_types.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/transcriptomics_features.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/proteomics_features.sql

mysql --user="root" --database="${MYSQL_DATABASE}" --password="${MYSQL_ROOT_PASSWORD}" < /mysql/reference_tables/users.sql

