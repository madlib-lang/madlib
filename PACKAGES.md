# Packages and package manager

## Introduction
Madlib package system is a flexible and decentralized system. That means that
you can install a package from different sources and refer to it via different
names in order to solve name-clashing.

## madlib.json
### Overview
A typical `madlib.json` file is defined like this:
```json
{
  "name": "PackageName",
  "version": "se.mv.er",
  "main": "src/Lib.mad",
  "bin": "src/Main.mad",
  "dependencies": {
    "MadDoc": "https://github.com/madlib-lang/maddoc/archive/refs/heads/master.zip"
  }
}
```

### keys
#### name
This is mainly metadata for now and serves to define a name for the package.

#### version
The current version of the package, it is recommended to follow semver.

#### main
Path to the module that export public functions. This is what is visible to
consumers of the package using imports.

#### bin
Path to the module that is supposed to be run. It would serve to call your app
if it is say a desktop app or CLI app/tool.

#### dependencies
An object where keys are the name of the package, and the value is a path to an
archive of that package. Since a URL is unique, it is recommended for package
authors to never replace a URL with a newer version but instead generate new
URLs for each version ( github does this for instance using tags or commit
hashes ).
Although the name of a package and its URL don't need to connected, it is
recommended to use the name that is in the madlib.json of that package,
exception allowed when you want to use two packages with the same name. There
is currently nothing preventing you from doing it, but that may get stricter in
the near future.
From the example `madlib.json` above you could use the MadDoc dependency like
this:
```madlib
// the from "MadDoc" is what is defined as the key in the madlib.json file
import MadDoc from "MadDoc"

doSomethingWith(MadDoc)
```

## Package manager CLI
The package manager is available from the compiler as a sub command `install`.

### Basic usage
```madlib
madlib install
```
This sub command contains no option for now and only installs all packages
from the local `madlib.json` file. The package manager is still under active
development and will soon get its deserved extra features such as the
possibility to install a package directly from the command line, remove
packages. 


