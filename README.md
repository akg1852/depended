# depended

Depended is a tool for mapping from .NET projects to the deployables which depend on them.

## Setup

* build the program using ghc
* generate sqlite database using the data.sql file: `sqlite3 data.db < data.sql`
* set configuration details in config.json

## Usage

`depended [options]`

Depended is a command line tool, with the following options available:

```
--clear             clear cache
--update            update cached data, by querying github
--search <query>    return results for specific project(s)
                    (% can be used as a wildcard)
--immediate         show immediate reverse-dependencies
                    instead of ultimate deployables
--plain             print without colours
```

## Assumptions

Certain assumptions were made in the design:
* all projects are assumed to be in a directory with the same name as the assembly they generate, and this directory should be directly in a directory named `project` at the top level of the repository
* projects are assumed to be deployable iff their directory contains a deploy.xml file
* projects are assumed to have a csproj/fsproj/sqlproj file, and (if they have nuget dependencies) a packages.config file
