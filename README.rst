..
.. In restructured text, you declare a section heading by writing underlines
.. on the next line.  The underlines must be _at_ _least_ as long as the
.. section heading.
..
.. Which underlines you use for each heading are defined by the order they
.. appear in your document.  We are using:
.. H1 ===
.. H2 ---
.. H3 ~~~
..

.. role:: red
.. role:: green
.. role:: blue
.. role:: orange

================================================================================
STSDAS/TABLES Version 3.18.3, STECF Version
================================================================================

:abstract:

    This is a single tar file distribution of STSDAS, TABLES, and STECF.
    It is considerably easier to install than previous versions.

.. contents::


Installing Binaries into Your IRAF Environment (Macintosh or Linux)
--------------------------------------------------------------------------------

Download the tar file and run these commands:

 ::

        tar zxf stsci_iraf-3.18.3.tar.gz
        cd stsci_iraf-3.18.3
        ./install_helper

The install_helper script will show you the lines to add to extern.pkg.
If your extern.pkg file already contains definitions for STSDAS,
TABLES, or STECF, remove them.



Installing Binaries As a Macintosh Package
--------------------------------------------------------------------------------

There is a complete environment of IRAF, Python, STSDAS, TABLES, STECF,
and STSCI_PYTHON available as a Macintosh package.  See the STSCI_PYTHON
instructions for details how to install that package.


Installing From Source
--------------------------------------------------------------------------------

First, consider whether you really want to do this.  Installing an IRAF package
from source is usually an elaborate procedure that most users will want to
avoid if possible.

Some good reasons to install from source are:

 - you are using an operating system other that Linux or Macintosh,
   so there are no binaries available

 - you need to modify the source code







Assistance
--------------------------------------------------------------------------------

If you have any difficulties, please do not hesitate to contact us
for assistance.  If you have questions or suggestions contact us
at help@stsci.edu.


