[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![CRAN status](https://www.r-pkg.org/badges/version-last-release/ChemoSpec)]() [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/ChemoSpec)]()  [![Build & Check](https://github.com/bryanhanson/ChemoSpec/workflows/Build-Check/badge.svg)]() [![Docs Current](https://github.com/bryanhanson/ChemoSpec/workflows/Update-Docs/badge.svg)]() [![Downloads](https://cranlogs.r-pkg.org/badges/ChemoSpec)]() [![status](https://tinyverse.netlify.com/badge/ChemoSpec)]()

## readJDX -- Import data in the JCAMP-DX format.

Import data written in the JCAMP-DX format. This is an instrument-independent format used in the field of spectroscopy. Examples include IR, NMR, and Raman spectroscopy. See the vignette for background and supported formats.

`readJDX` is tested against a wide variety of JCAMP-DX formatted files provided by collaborators and available in various places on the internet. Details about supported formats, known issues and literature references are available in the vignette.

## How to install readJDX

### From CRAN using R:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("readJDX")
library("readJDX")
````

### To install from Github using R:

````r
install.packages("remotes")
library("remotes")
install_github(repo = "bryanhanson/readJDX@master")
library("readJDX")
````
If you use `@devel` you can get the development branch if it is available (and there may be other branches out there too).  Development branches have new, possibly incompletely tested features.  They may may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.

### To view the Vignettes:

````r
browseVignettes("readJDX")
````
### License Information

`readJDX` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
