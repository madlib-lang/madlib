## Package structure
A madlib package consists of:
- a madlib.json file describing the properties of the project such as the main module which exports all public functions and types
- a source folder
- a main module

### Example
A fictive MadServer library to build http servers could be defined as follows:

{projectRoot}/madlib.json
{projectRoot}/src/Main.mad

with a madlib.json with the following content:
```json
{
  "name": "MadServer",
  "version": "0.0.1",
  "madlibVersion": "0.12.2",
  "main": "src/Main.mad"
}
```

## madlib.json

### File format
#### **name**: string
Name of the package.

#### **version**: string
Current version of the package.

#### **main**: string
Path to the main module, relative to the location of the madlib.json file.

#### **bin**: string
Path to the main module to be executed, used by `madlib run` command.

#### **madlibVersion**: string
Minimum Madlib version the project runs with.

#### **importAliases**: object
Definition of import aliases. The keys are the aliased paths and the values are the target of the alias. For example:
```json
"importAliases": {
  "views": "src/views"
}
```
would allow one to use `import ... from "@views/AView"` from anywhere within the project. Not that the special alias `.` resolves to `@`.

#### **dependencies**: array
Dependencies that should be installed via `madlib install` package installer. The array consists of object entries defined as follows:
minVersion: string - lowest version of the package required
maxVersion: string - highest version of the package required
url: string - a url pointing to the zip of the project ( most likely a github release )
description: string - a description field to explain why the dependency is needed or a quick note about what it does

Only the url field is mandatory.

### Example
```json
{
  "name": "MadServer",
  "version": "0.0.1",
  "madlibVersion": "0.12.2",
  "main": "src/Main.mad",
  "importAliases": {
    ".": "src"
  },
  "dependencies": [
    {
      "minVersion": "0.0.4",
      "url": "https://github.com/madlib-lang/madmarkdown-renderer/archive/refs/tags/v0.0.4.zip",
      "description": "MadMarkdownRenderer"
    }
  ]
}
```


## Releasing a package
Once you are ready to release a new version of a package, you should run the `madlib package` command. This will run an analysis of the package, making sure that it compiles, and create or update a `version.lock` file containing information about the public API of the package as well as build hashes to make sure that what you install is what was released. It also automatically bumps the version in the `madlib.json` file based on the API changes that were detected. It follows semver, which implies that existing exported functions that changed or were deleted trigger a major version bump, function additions generate a minor version bump and if no API was changed a patch version bump.

## Installing a package
To install a package you should add it to the dependencies of your `madlib.json` file as described above, and run `madlib install` from the root of your project ( where the madlib.json file is located ). This will also cause required dependency version ranges to be checked, as well as build hashes. If versions don't match it'll still run through but emit warnings.
