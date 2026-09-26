# SigRepo_Server 1.0.0
* Documentation: https://montilab.github.io/SigRepo_Server/
* GitHub: https://github.com/montilab/SigRepo_Server/
* `/init_db` and the other admin routes report the failing call alongside the error message instead of a flattened string; the orphaned `mysql/schema/snps_features.sql`, which `generate_db_schema()` never created, is removed. The `/init_db` failure on databases not named `sigrepo` was a client bug, fixed in SigRepo #245 (#130).


