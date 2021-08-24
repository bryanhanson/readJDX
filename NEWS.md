# readJDX 0.5.? (2021-08-11)
## Enhancements
* `readJDX` can now read the `##XYPOINTS=(XY..XY)` format.
* Now using `pkgdown` for html documentation.
* Vignettes improved, including new vignette "Taxonomy".

## Behind the Scenes
* `processPT` renamed to `processXYXY` as part of simplifying the reading of any format that contains one x,y pair per line in AFFN format.
* `processDT` renamed to `processLCMS` to better reflect what it does.
* Function `process2DNMR` and `processXYY` re-worked to simplify and generalize the processing workflow.
* See the "Taxonomy" vignette for a call graph of the functions.

# readJDX 0.5.61 (2021-08-07)
## Enhancements
* Support for at least some forms of LC-MS/GC-MS data in NTUPLES format added.  Works on a limited number of test files.  Uses new function `processDT` to parse data sets with the `##DATA TABLE= (XI..XI)` format.

## Behind the Scenes
* Additional comments to the code to better document the process.

# readJDX 0.5.41 (2021-04-16)
## Misc.
* Update `DESCRIPTION` per CRAN request.

# readJDX 0.5.36 (2021-03-15)
## Enhancements
* New function `splitMultiblockDX` will process a multiblock JCAMP-DX files into individual JCAMP-DX files.  Requested by multiple users.

# readJDX 0.5.29 (2020-07-19)
## Enhancements
* New function `processPT` parses data sets with the `##PEAK TABLE= (XY..XY)` format.
## Misc.
* Function `decompressXYY` renamed to `processXYY` as part of the introduction of `processPT`.

## Behind the Scenes
* Refactor the use of `mode` and `fmt` to be more consistent and leaner, as part of the introduction of the PEAK TABLE processing.
* Additional MS files using PEAK TABLE format added to local test suite (not publically available).

# readJDX 0.4.29 (2019-11-12)
## Enhancements
* New function `decompLines` handles the decompression process (and is called by `decompressXYY`).  The function can also be used in stand alone fashion to inspect problematic lines in a particular JCAMP-DX file.
* New function `checkYvalues` added, which handles the task of checking Y values in DIF mode.
* New function `sinkall` added.
* Some internal functions re-named.
* Existing vignette re-worked, and new vignettes added.
* New test files added.
* Format of this NEWS file changed to NEWS.md.
* Lots of additional documentation in the help files and in the code itself.
* New function `.onAttach` added to provide start up message.
* The items above constitute a considerable re-write with much simplification in the processing.  We are now at version 0.4.x series.
## Note
* This version is successfully tested against > 100 files in our test suite, including a number of 2D NMR spectra.  However, there are still files that cannot be parsed successfully.  It appears least some of these are due to vendor's errors.  Others may fail due to problems with `readJDX`.  Please submit an issue at Github when you have a file that can't be read.

# readJDX 0.3.372 (2019-09-16)
* This version was devel branch only, not released to CRAN.
* Debug options are now more consistent, documentation improved, and a new debug level was added.
* Checking of first and last x and y values was relaxed a bit, and made more sensible.  Documentation was improved.
* Fixed a bug in which DUP codes longer than a single character (e.g. "S2") returned the wrong answer.
* Internal handling of mode/fmt was simplified and consistently named.
* Argument `lineNos` was eliminated from most functions by naming the lists passed in.
* `decompressJDXxyy` renamed `decompressXYY`.
* Fixed an issue with extractParams where NMR imaginary data could be missed, based on a nuance of Bruker formatting.
* Fixed a bug in which the y value check, required by the standard, was carried out if *any* DIF codes were present in a given line of the input file.  In fact, the check should be carried out *only if the last value on the line* was a DIF code.  Reported with careful documentation and a test file by Daniel Jacob, in Github Issue #6.  A big thank you to Daniel!
* In the course of testing the file supplied by Daniel Jacob, discovered that a DIF code followed by a SQZ code was not handled at all (e.g. MH111).  Fixed this too.
* Reading 2D NMR formats is still a work in progress.

# readJDX 0.3.250 (2018-10-15)
* Checked against R devel r75432 which is R 3.5.1 patched.
* README.md updated.
* Checked against testbank.
* ORCID added to DESCRIPTION.
* Minor update to the vignette.

# readJDX 0.3.248 (2018-08-24)
* Checked against R devel r75161 which is R 3.5.1 patched.

# readJDX 0.3.247 (2018-06-30)
* Rebuilt how duplicates are handled, so that S2, which is a valid DUP string = 12, is handled correctly.

# readJDX 0.3.215 (2017-01-24)
* Improvements to vignette and documentation.
* Improvements to the debugging output options.

# readJDX 0.3.194 (2017-01-23)
* Return structure now includes the comment only lines found outside of the metadata.
* Much more robust handling of comments.
* All test files seem to work.
* Still a lot of polishing needed.

# readJDX 0.3.103 (2017-01-13)
* Return structure now includes a data guide giving the lines where each piece of the file was parsed (improves error reporting, but needs further tweaking).
* Will now read 2D NMR spectra in NTUPLES format (at least in the only case I have available right now).
* Updated vignette to pinp style.  

# readJDX 0.2.1 (2016-12-21)
* Fixed issue #3, problem with x values not having the correct absolute value.  Reported by Rustam.
* Added checks for FIRSTX, LASTX.
* Added PCRF.JDX, SBO.JDX and SBO.JDX to inst/extdata for example purposes.
* Internal tolerance checking was made more precise.
* Switched to using separate x and y tolerances internally.

# readJDX 0.1.32 (2016-10-21)
* findDataTable renamed to findDataTables to better reflect what it does.
* Added an argument SOFC with default TRUE.  SOFC = stop on failed check.  As currently implemented, it skips the FIRSTY check.  Suggested by Rustam as some JCAMP writers do not put FIRSTY into the file.
* Many additions to the decompression code based on additional testing, which uncovered some previously unseen issues.  There are still some files that won't process; these will require time to troubleshoot.

# readJDX 0.1.0 (2016-08-19)
* First draft of package.  Works on a wide variety of example files.
* First release to GitHub.
