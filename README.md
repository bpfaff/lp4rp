# Introduction

This project is inspired by the keynote
talk
[Literate Programming](http://schedule.user2016.org/event/7BaH/literate-programming) given
by Donald Knuth at the [useR!](http://user2016.org/) conference. The
goal is to provide a show-case on how literate programming can be used
for the creation of R packages. As such, the resultant R package
**hiker** (pronounce: 'hike-R') should be viewed as a toy-example for
an implementation of the methods proposed by Palshikar for peak
detection in time-series
(see
[Paper on Researchgate](https://www.researchgate.net/publication/228853276_Simple_Algorithms_for_Peak_Detection_in_Time-Series)). 

# Prerequisite

The processing of the [noweb](http://www.cs.tufts.edu/~nr/noweb/)-file
'hiker.Rnw' and the resultant package creation is accomplished by the
'Makefile'. In order to do so, the R packages **devtools** (for
roxygenizing the man-pages from the R-source files) and
**Rnoweb** (for processing the noweb-file) are required. The former
can be installed for instance from CRAN and the latter is provided as
a source tarball within this project for the user's
convenience. Unfortunately, **Rnoweb** has not been released on CRAN,
but a detailed description is made available on Ross Ihaka's
[Rnoweb-site](https://www.stat.auckland.ac.nz/~ihaka/software/Rnoweb/Rnoweb.html). Finally,
the LaTeX-file 'noweb.sty' is required for processing the
'hiker.tex'. This style file is part of
the [noweb](http://www.cs.tufts.edu/~nr/noweb/) bundle and provided
within this project, too. Hence, it should suffice to have a running R
installtion, cloning this project and running 'make'. 

# Status quo and ToDo's

By now, the skeleton of the 'noweb-file' has been created and the
'Makefile' has been drafted such that the package **hiker** is build
and checked and the 'pdf-file' of the package in terms of a literate
program is created. 
What remains to be done, is to finish the write-up of the package and
make the LP accessible as a vignette.


