Introduction

The deoxybyte-run system is a compatability and convenience layer for
running external programs. In addition, wrappers for some software are
included:

- Gnuplot


Installation

deoxybyte-run uses ASDF 3 for system definition. Add deoxybyte-run.asd
(and optionally deoxybyte-run-test.asd) to your set of ASDF systems as
described in the ASDF manual and load deoxybyte-run with the
asdf:load-system function:

 (asdf:load-system :deoxybyte-run)

Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:test-system function:

 (asdf:test-system :deoxybyte-run)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :deoxybyte-run)

at the REPL, provided that CLDOC is installed.


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git
deoxybyte-utilities     git://github.com/keithj/deoxybyte-utilities.git
deoxybyte-io            git://github.com/keithj/deoxybyte-io.git


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
