-------------------------
What is MissingPy?
-------------------------

It's two things:

1. A Haskell binding for many C and Python libraries for tasks such as
   data compression, databases, etc.  This can be found in the
   MissingPy module tree.

2. A low-level Haskell binding to the Python interpreter to
   enable development of hybrid applications that use both
   environments.  This can be found in the Python module tree.  The
   Haskell bindings above use this environment.

MissingPy permits you to call Python code from Haskell.  It does NOT
permit you to call Haskell code from Python.

-------------------------
Major Features
-------------------------

 * GZip and BZip2 compression and decompression using the generic
   Handle-like HVIO interface

 * *dbm persistent storage using the generic AnyDBM interface

 * Low-level interface to Python for extending your own Haskell code

 * Many unit tests to verify proper functionality

** THIS IS CURRENTLY BETA-QUALITY CODE; MAJOR API FLUCTUATIONS MAY YET OCCUR.

-------------------------
Quick Start
-------------------------

See the file INSTALL.

-------------------------
Usage in programs
-------------------------

You can simply use -package MissingPy in ghc to enable
this library.  

Note that you'll want to compile most of your programs with
-fallow-overlapping-instances at least.  (If you use *only*
MissingPy/*, that may not be necessary.)  Also, please note that you
must call Python.Interpreter.py_initialize before doing anything else.

The API docs can be built with "make doc", or you can find them at:

http://software.complete.org/missingpy

-------------------------
Author & Homepage
-------------------------

MissingPy was written by John Goerzen <jgoerzen@complete.org>.

The latest version may be obtained at:

   http://software.complete.org/missingpy

Documentation is also available on that page.

This program is copyrighted under the terms of the GNU General Public License.
See the COPYRIGHT and COPYING files for more details.

If the GPL is unacceptable for your uses, please e-mail me; alternative
terms can be negotiated for your project.

