BEGIN TRANSACTION;
CREATE TABLE project (name TEXT, repo TEXT, branch TEXT, path TEXT, deployable TEXT, packageshash TEXT);
CREATE TABLE relationship (parent TEXT, child TEXT);
COMMIT;
