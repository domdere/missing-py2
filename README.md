# missing-py2 [![Build Status](https://travis-ci.org/domdere/missing-py2.png?branch=master)](https://travis-ci.org/domdere/missing-py2)

A fork of John Goerzen's [**MissingPy**] [softmechanics-missingpy]

Updated to run with **GHC 7.6.3** and run with **Python 2.7.3**, for maintaining projects with existing MissingPy dependencies.

see the original documentation for that project [**here**](./MissingPy.md).

# Usage

Just add `missing-py2` to your `build-depends` list, e.g:

    build-depends:
            base < 5 && >= 4
        ,   missing-py2

The original documentation prescribes that the `OverlappingInstances` **GHC** extension is neccessary anywhere you use the `Python.*` modules.

That **may** be neccessary, however the unit tests seem to run fine without it now.

Don't forget to run `py_initialize` from `Python.Interpreter` during the start up phase of your application

# See Also

-   [**softmechanics/missingpy**] [softmechanics-missingpy] : The original project
-   [**cpython**](http://hackage.haskell.org/package/cpython "cpython on hackage") : Another well-maintained set of `libpython` bindings,
    this would probably be recommended for new projects.


[softmechanics-missingpy]: https://github.com/softmechanics/missingpy "softmechanics/missingpy on GitHub.com"
