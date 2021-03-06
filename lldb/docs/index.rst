.. title:: LLDB Homepage

The LLDB Debugger
=================

Welcome to the LLDB version |release| documentation!

LLDB is a next generation, high-performance debugger. It is built as a set of
reusable components which highly leverage existing libraries in the larger LLVM
Project, such as the Clang expression parser and LLVM disassembler.

LLDB is the default debugger in Xcode on macOS and supports debugging C,
Objective-C and C++ on the desktop and iOS devices and simulator.

All of the code in the LLDB project is available under the
`"Apache 2.0 License with LLVM exceptions"`_.

.. _"Apache 2.0 License with LLVM exceptions": https://llvm.org/docs/DeveloperPolicy.html#new-llvm-project-license-framework

Why a New Debugger?
-------------------

In order to achieve our goals we decided to start with a fresh architecture
that would support modern multi-threaded programs, handle debugging symbols in
an efficient manner, use compiler based code knowledge and have plug-in support
for functionality and extensions. Additionally we want the debugger
capabilities to be available to other analysis tools, be they scripts or
compiled programs, without requiring them to be GPL.

Compiler Integration Benefits
-----------------------------

LLDB currently converts debug information into clang types so that it can
leverage the clang compiler infrastructure. This allows LLDB to support the
latest C, C++, Objective-C and Objective-C++ language features and runtimes in
expressions without having to reimplement any of this functionality. It also
leverages the compiler to take care of all ABI details when making functions
calls for expressions, when disassembling instructions and extracting
instruction details, and much more.

The major benefits include:

- Up to date language support for C, C++, Objective-C
- Multi-line expressions that can declare local variables and types
- Utilize the JIT for expressions when supported
- Evaluate expression Intermediate Representation (IR) when JIT can't be used

Reusability
-----------

The LLDB debugger APIs are exposed as a C++ object oriented interface in a
shared library. The lldb command line tool links to, and uses this public API.
On macOS the shared library is exposed as a framework named LLDB.framework,
and unix systems expose it as lldb.so. The entire API is also then exposed
through Python script bindings which allow the API to be used within the LLDB
embedded script interpreter, and also in any python script that loads the
lldb.py module in standard python script files. See the Python Reference page
for more details on how and where Python can be used with the LLDB API.

Sharing the LLDB API allows LLDB to not only be used for debugging, but also
for symbolication, disassembly, object and symbol file introspection, and much
more.

Platform Support
----------------

LLDB is known to work on the following platforms, but ports to new platforms
are welcome:

* macOS desktop user space debugging for i386 and x86_64
* iOS simulator debugging on i386 and x86_64
* iOS device debugging on ARM and AArch64
* Linux local user-space debugging for i386, x86_64 and PPC64le
* FreeBSD local user-space debugging for i386 and x86_64
* Windows local user-space debugging for i386 (*)

(*) Support for Windows is under active development. Basic functionality is
expected to work, with functionality improving rapidly.

Get Involved
------------

Check out the LLVM source-tree with git and find the sources in the `lldb`
subdirectory:

::

  > git clone https://github.com/llvm/llvm-project.git

Note that LLDB generally builds from top-of-trunk using CMake and Ninja.
Additionally it builds:

* on macOS with a :ref:`generated Xcode project <CMakeGeneratedXcodeProject>`
* on Linux and FreeBSD with clang and libstdc++/libc++
* on NetBSD with GCC/clang and libstdc++/libc++
* on Windows with a generated project for VS 2017 or higher

See the :doc:`LLDB Build Page <resources/build>` for build instructions.

Discussions about LLDB should go to the `lldb-dev
<http://lists.llvm.org/mailman/listinfo/lldb-dev>`__ mailing list. Commit
messages are automatically sent to the `lldb-commits
<http://lists.llvm.org/mailman/listinfo/lldb-commits>`__ mailing list , and
this is also the preferred mailing list for patch submissions.

See the :doc:`Projects page <status/projects>` if you are looking for some
interesting areas to contribute to lldb.

.. toctree::
   :hidden:
   :maxdepth: 1
   :caption: Project

   status/goals
   status/features
   status/status
   status/projects
   status/releases

.. toctree::
   :hidden:
   :maxdepth: 1
   :caption: Use & Extension

   use/tutorial
   use/map
   use/formatting
   use/variable
   use/symbolication
   use/symbols
   use/python
   use/python-reference
   use/remote
   use/troubleshooting
   use/architecture

.. toctree::
   :hidden:
   :maxdepth: 1
   :caption: Development

   resources/contributing
   resources/build
   resources/test
   resources/bots
   resources/sbapi

.. toctree::
   :hidden:
   :maxdepth: 1
   :caption: API Documentation

   Public Python API Reference <https://lldb.llvm.org/python_reference/index.html>
   Public C++ API Reference <https://lldb.llvm.org/cpp_reference/namespacelldb.html>
   Private C++ Reference <https://lldb.llvm.org/cpp_reference/index.html>

.. toctree::
   :hidden:
   :maxdepth: 1
   :caption: External Links

   Source Code <https://github.com/llvm/llvm-project>
   Code Reviews <https://reviews.llvm.org>
   Bug Reports <https://bugs.llvm.org/>
