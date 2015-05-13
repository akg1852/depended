BEGIN TRANSACTION;

CREATE TABLE project (name TEXT, repo TEXT, branch TEXT, path TEXT, deployable TEXT, packageshash TEXT);
INSERT INTO project VALUES('Xyz.Abc','Foo/Xyz','master','project/Abc',NULL,'123456');
INSERT INTO project VALUES('Xyz.Def','Foo/Xyz','master','project/Def','Def','654321');
INSERT INTO project VALUES('Xyz.Ghi','Foo/Xyz','master','project/Ghi',NULL,'998877');

CREATE TABLE relationship (parent TEXT, child TEXT);
INSERT INTO relationship VALUES('Xyz.Def','Xyz.Abc');
INSERT INTO relationship VALUES('Xyz.Def','Xyz.Ghi');

COMMIT;
