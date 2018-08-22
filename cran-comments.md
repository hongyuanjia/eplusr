# Version 0.8.1

## Round 1
### Test environments
* local Windows 10 install, R 3.5.0
* Windows (on appveyor-ci), R oldrel, patched, stable, and release
* ubuntu 14.04 (on travis-ci), R oldrel, release, devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results
0 errors | 0 warnings | 1 notes

There is one note which indicates this is the first submission:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Hongyuan Jia <hongyuanjia@cqu.edu.cn>'
```

### Reverse dependencies
eplusr currently has no reverse dependencies.

### Reviewer comments
2018-07-17 Uwe Ligges:

```
   Possibly mis-spelled words in DESCRIPTION:
     EnergyPlus (2:28, 10:60, 11:66)

   The Description field should not start with the package name,
     'This package' or similar.

Also, file LCIENSE should  be omitted, if it is just a copy of the GPL-3.

Please fix and resubmit.
```

## Round 2
### Submission comments
2018-07-17

Addressed all privious comments.

# Version 0.8.2

## Round 1
### Reviewer comments
2018-07-22 Swetlana Herbrandt

```
Thanks, please omit the redundant "in R" in your title.

Please write package names, software names and API names in single 
quotes (e.g. 'EnergyPlus') in Title and Description.

Please fix and resubmit.

Best,
Swetlana Herbrandt
```

# Version 0.8.3
## Round 1
### Test environments
* local Windows 10 install, R 3.5.0
* local Manjaro Linux 17.1.11.1 install, R 3.5.1
* Windows (on appveyor-ci), R oldrel, patched, stable, and release
* Ubuntu 14.04 (on travis-ci), R oldrel, release, devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results
0 errors | 0 warnings | 0 notes

### Reverse dependencies
eplusr currently has no reverse dependencies.

### Reviewer comments
2018-08-07 Uwe Ligges

```
This hangs on Windows.

Please verify that this works on Windows before submitting to CRAN, or 
if not possible, declare an OS_type.

Please fix and resubmit.

Best,
Uwe Ligges
```

### Submission comments
2018-08-07

Fix the function that install EnergyPlus which hangs on the tests when there is
no powershell installed on Windows machine.

# Version 0.8.4

## Round 1

### Test environments

* local Manjaro Linux 17.1.11.1 install, R 3.5.1
* Windows (on appveyor-ci), R oldrel, release and devel
* Ubuntu 14.04 (on travis-ci), R oldrel, release and devel
* OS X (on travis-ci), R oldrel and release

### CRAN comments

2018-08-08

```
The package was archived by CRAN due to leaving 'EnergyPlus' installer file and
installation in home.
```

### Submission comments

2018-08-09

Skip tests which download and install 'EnergyPlus' in home folder on CRAN and
only run those tests locally.

# Version 0.9.0

## Round 1

### Test environments

* local Manjaro Linux 17.1.11.1 install, R 3.5.1
* Windows (on appveyor-ci), R oldrel, release and devel
* Ubuntu 14.04 (on travis-ci), R oldrel, release and devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results
0 errors | 0 warnings | 1 note

```
Maintainer: 'Hongyuan Jia <hongyuanjia@cqu.edu.cn>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2018-08-08 for policy violation.

  Leaves 'EnergyPlus' installer file and installation in home.
```

### Reverse dependencies
eplusr currently has no reverse dependencies.

### Last reviewer comments

2018-08-09 Uwe Ligges

```
Thanks, some more things to fix:

1. your *only* relevant example is

> run_idf(8.8, "input.idf", "weather.epw")

which gives
Error in normalizePath(path.expand(path), winslash, mustWork) :
  path[1]="input.idf": Das System kann die angegebene Datei nicht finden

on my machine: Can't you add the two required files to the package so that users can really execute your example?

2. Can you pls add a web reference pointing to EnergyPlus in the form <http....> to the Description field of the DESCRIPTION file?

3. Although you do not generate files in the user fikespace during the checks any more, some functions write by default to the user space or even delete files from the user space.

Although some are intended to do so, can't you remove the default and simply let the user specify the directory that is to be used? If you really need a default (which I doubt), use tempdir(). 
```

### Submission comments

1. Examples have been added to all exported functions and classes. An EnergyPlus
   v8.8 model named `1ZoneUncontrolled.idf` has been added and shipped with the
   package, which enables users to successfully run the main function
   `read_idf()` and thus all `Idf` and `IdfObject` class methods. It is worth
   noting that parsing an EnergyPlus model also needs an EnergyPlus Input Data
   Dictionary (IDD) file which usually has a size of 4 MB. In order to reduce
   the package size, IDD file is not shipped but using `download_idd()` to get
   it from EnergyPlus GitHub repo. Also, `run_idf()` and other functions which
   call EnergyPlus require EnergyPlus to be installed, which is usually 100 MB.
   EnergyPlus has been added into "SystemRequirements", Please see 2.

2. A web reference "(<https://energyplus.net>)" has been added into the
   Description field in the DESCRIPTION file. Also,
   "EnergyPlus (<https://energyplus.net>)" has been added into the
   "SystemRequirements" field in the DESCRIPTION file.

3. All default directory parameters in functions that manipulate local files have
   been moved, which always ask users to explicitly specify. Those functions
   include `run_idf()`, `run_multi()`, `$run()` method in `Idf` class,
   `EplusJob` class and `ParametricJob` class, `download_eplus()`,
   `download_idd()`. The parameter `dir` has been removed in `install_eplus()`.
   The downloaded EnergyPlus installer will be saved into `temper()`.
