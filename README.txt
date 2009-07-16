Introduction

The deoxybyte-run system is a compatability and convenience layer for
running external programs. In addition, wrappers for some software are
included:

- Gnuplot
- rsh


Installation

deoxybyte-run uses ASDF for system definition. Copy or symlink
deoxybyte-run.asd (and optionally deoxybyte-run-test.asd) to your
asdf:*central-registry* and load deoxybyte-run with the asdf:operate
function:

 (asdf:operate 'asdf:load-op :deoxybyte-run)

or with the equivalent deoxybyte-systems:load-system function:

 (dxs:load-system :deoxybyte-run)


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:operate function:

 (asdf:operate 'asdf:test-op :deoxybyte-run)

or with the equivalent deoxybyte-systems:test-system function:

 (dxs:test-system :deoxybyte-run)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :deoxybyte-run)

at the REPL, provided that CLDOC is installed.


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git
deoxybyte-utilities     git://github.com/keithj/deoxybyte-utilities.git
deoxybyte-io            git://github.com/keithj/deoxybyte-io.git
