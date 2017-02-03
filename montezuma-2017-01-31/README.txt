Copyright 2006 John Wiseman <jjwiseman@yahoo.com> 7/13/2006
Copyright 2016 Roy Anderson <reanz1959@gmail.com> 2016-10-05

Last Updated 2016-10-05

Welcome to Montezuma! A Common Lisp, open source system for indexing, searching, and analyzing large document collections.

Montezuma provides a comprehensive solution for full-text index and search systems that works in concert with other code systems. It also serves as a code base worthy of study including the use of Common Lisp Object System (CLOS) and a purpose built query languages. Montezuma is capable of handling large scale applications with corpus management. Unless you have an interest in a Common Lisp full text index library, Montezuma is probably not for you.

Introduction

Montezuma is based on the Ruby Ferret code library which is itself based on the Lucene library (http://lucene.apache.org/) implemented in Java.

What's New?

Montezuma-2016-10-5 introduces an text extraction (or summarisation) with abstract-documents.
It also introduces phrase identification which finds reoccuring sequences of terms with make-phrases.

Mpntezuma 2.0.1 introduces improved handling of invalid queries: instead of substituting a simple analysis of query terms, Montezuma now raises an error condition and does not attempt to interpret queries that cannot be parsed. 

Montezuma now includes typed queries. More specifically, when a field definition in an index identifies a field type, the type guides the query parser's interpretation of targeted values. So far, supported field consist of: date, int, or float.

Montezuma 1.2.0 brings a Listener based shell (Read Eval Print Loop) for exploring Montezuma including the query parser and search features. It also introduces Oropendola: a [LispWorks](http://www.lispworks.com/) GUI alternative to the Listener shell. To start the shell, copy the Montezuma source and dependencies
and enter `(shell)`. To start the Oropendola GUI, you may need to install LispWorks then enter `(oropendola)` in the Listener.

Dependencies

Montezuma requires other systems, but they are now incorporated into the Montezuma release in the dependents directory.

To start up Montezuma, load systems.lisp.

Installation Guide

After downloading the Montezuma release file, edit the systems.lisp file and make sure the root directory exists
so that you can load montezuma. You may also need to set the index path. It's still a little cumbersome but it should work relatively well after you have configured the directory paths. When these changes have been made you may still have to compile and load the systems.lisp file when you want to start using Montezuma.

Montezuma has been tested with Lispworks 6.1.1 and 7.0 (Windows 10), SBCL 0.9.12 (OS X/PPC), SBCL 0.9.13 (Linux/x86) OpenMCL 1.0 (OS X/PPC) and ACL 8.0 (OS X/PPC). It has been extended in 2015 using Lispworks 6.1.1.

The only implementation-dependent code in Montezuma is in src/util/mop.lisp. To add support for another implementation may be as simple as adding one line to the definition of the CLASS-SLOTS function and one to SLOT-DEFINITION-NAME.

Installation and Loading

You can use ASDF-INSTALL to install Montezuma:

~~~~
  (asdf-install:install '#:montezuma)
~~~~

And ASDF to load it:

~~~~
  (asdf:oos 'asdf:load-op '#:montezuma)
~~~~

Testing

Once Montezuma has been loaded, you can run the unit tests if you like:

~~~~
  (asdf:oos 'asdf:test-op '#:montezuma)
~~~~

Use

See the TUTORIAL.TXT file for more information on how to use Montezuma.

The Montezuma project page at http://projects.heavymeta.org/montezuma/
should have the latest information about Montezuma.

Acknowledgements

Thanks to Dave Balmain, Gary King, Peter Seibel (for his META-inspired parser), Xach Beane (for the heap implementation from his ([TIMER](http://www.xach.com/lisp/timer/doc.html)) library[1]) and Franz. Inc. (for their ([Porter stemmer](http://www.lispwire.com/entry-text-porter-word-stemmer-des)).

Failures and Successes Adding Montezuma Documents

[REA] While adding documents to Montezuma, every dozen or so additions would raise a Delete File or Rename File exception. I retried adding documents (for Rename exceptions) or restart the load from the last document added. This problem disappeared when I moved the index directory from a DropBox networked drive to a local drive. Not only did the exceptions disappear, but the load times improved from about 5 hours to 5 minutes. I could also remove the checkpoints and repeated index optimization without exceptions.

Onward

For a complete example of using Montezuma to index and retrieve real information, see the file `tests/corpora/pastes-1000/paste-search.lisp`.

