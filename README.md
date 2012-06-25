Clafer
======

[Clafer](http://clafer.org) is a general purpose lightweight modeling language developed at [GSD Lab](http://gsd.uwaterloo.ca/), [University of Waterloo](http://uwaterloo.ca). Lightweight modeling aims at improving the understanding of the problem domain in the early stages of software development and determining the requirements with fewer defects. The main goal of Clafer is to make modeling more accessible to a wider range of users and domains. 

Clafer Compiler
===============

Clafer compiler provides a reference language implementation. It translates models in Clafer to other formats (e.g. Alloy, XML) to allow for reasoning with existing tools.

Currently, the compiler is used by Clafer Instance Generator ([ClaferIG](https://github.com/gsdlab/claferIG)) and Clafer Wiki ([ClaferWiki](https://github.com/gsdlab/claferwiki)).

Dependencies
------------
* [The Haskell Platform](http://hackage.haskell.org/platform/) v.2012.2.0.0
* [Java Platform (JDK)](http://www.oracle.com/technetwork/java/javase/downloads/index.html) >= 5, 32bit
* [Alloy4.1 and/or Alloy4.2-rc](http://alloy.mit.edu/alloy/download.html)
  * downloaded automatically  
* [Git](http://git-scm.com/)

On Windows only

* [Cygwin](http://www.cygwin.com/) with packages `make`, `wget`, `unzip`

Building & Installation
-----------------------

h3. Important: Branches must correspond

Clafer and ClaferIG are following the  *simultaneous release model*. 
The branch `master` contains releases, whereas the branch `develop` contains code under development. 
When building the tools, the branches should match:
Releases `clafer/master` and `claferIG/master` are guaranteed to work well together.
Development versions `clafer/develop` and `claferIG/develop` should work well together but this might not always be the case.

h3. Building

1. install the dependencies
2. in some `<source directory>` of your choice, execute `git clone git://github.com/gsdlab/clafer.git`
3. in `<source directory>/clafer`, execute
  * `cabal update`
  * `make`

h3. Installation

1. execute `make install to=<target directory>`
2. add the `<target directory>` is on your command PATH

#### Note: 
> On Windows, use `/` with the `make` command instead of `\`.

Usage
-----

(As printed by `clafer --help`)

```
Clafer v0.3.25-6-2012

clafer [OPTIONS] [FILE]

Common flags:
  -m --mode=CLAFERMODE       Generated output type. Available modes are:
                             'alloy' (default, Alloy 4.1); 'alloy42' (Alloy
                             4.2-rc); 'xml' (intermediate representation of
                             Clafer model); 'clafer'  (analyzed and desugared
                             clafer model)
  -o --console-output        Output code on console
  -i --flatten-inheritance   Flatten inheritance
     --timeout-analysis=INT  Timeout for analysis
  -l --no-layout             Don't resolve off-side rule layout
  -n --nl --new-layout       Use new fast layout resolver (experimental)
  -c --check-duplicates      Check duplicated clafer names
  -f --skip-resolver         Skip name resolution
  -k --keep-unused           Keep uninstantated abstract clafers
  -s --no-stats              Don't print statistics
     --schema                Show Clafer IR (intermediate representation) XML
                             schema
  -v --validate              Validate output. Uses 'tools/XsdCheck.class' for
                             XML,  'tools/alloy4.jar' and
                             'tools/alloy4.2-rc.jar' for Alloy models, and
                             Clafer translator for desugared Clafer models. Use
                             --tooldir to override the default location of
                             these tools.
     --tooldir=DIR           Specify the tools directory. Default: 'tools/'
  -a --alloy-mapping         Generate mapping to Alloy source code
  -? --help                  Display help message
  -V --version               Print version information
```

Additionally, `[OPTIONS]` can also be specified directly in the model file by inserting the following as the first line of the file:

```
//# [OPTIONS]
```

for example

```
//# --keep-unused -m=alloy42
```

Options given at command line override the options given in the file using `//#` which, in turn, override the defaults.


Need help?
==========
* See [Project's website](http://gsd.uwaterloo.ca/clafer) for news, technical reports and more
  * Check out a [Clafer tutorial](http://gsd.uwaterloo.ca/node/310)
  * Try [Online translator](http://gsd.uwaterloo.ca/clafer/translator)
* Take a look at incomplete [Clafer wiki](https://github.com/gsdlab/clafer/wiki)
* Browse example models in the [test suite](https://github.com/gsdlab/clafer/tree/master/test/positive) 
* Post questions, report bugs, suggest improvements [GSD Lab Bug Tracker](http://gsd.uwaterloo.ca:8888/questions/). Tag your entries with `clafer` (so that we know what they are related to) and with `kacper-bak` or `jimmy-liang` (so that Kacper or Jimmy gets a notification).