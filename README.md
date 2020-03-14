ST-JSON
=======

ST-JSON ('ST' because it originated at
[Streamtech](http://streamtech.nl)) is a Common Lisp library for
encoding and decoding JSON values (as specified on
[json.org](http://json.org)).

This library does mostly the same thing as
[CL-JSON](http://common-lisp.net/project/cl-json/), but is simpler and
more precise about types (distinguishing boolean false, the empty array,
and the empty object).

Download and installation
-------------------------

The library is available under a [zlib-style](LICENSE) license.

The easiest way to install is with [QuickLisp](https://www.quicklisp.org):

    (ql:quickload :st-json)

It can also be installed with [asdf-install](http://www.cliki.net/ASDF-Install),
or you can download the current
[release](http://marijnhaverbeke.nl/st-json/st-json.tgz) manually. The
[git](http://git-scm.com) repository can be checked out with:

    git clone http://github.com/marijnh/ST-JSON


Documentation
-------------

See the webpage for [documentation](https://marijnhaverbeke.nl/st-json/).
