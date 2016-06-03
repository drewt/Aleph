λleph
=====

Copyright © 2016 Drew Thoreson

λleph is a feed aggregator (RSS/Atom) daemon with an HTTP interface,
designed with extensibility in mind.

λleph is not yet in a ready-to-use state.  Most of the basic features are
working in the back-end, but the provided client is woefully incomplete and
there are many important and not-so-important features still to be added to
the server.

Technical Info
--------------

λleph is written in Common Lisp.  Currently it will only run on SBCL, as it
uses some extensions provided by that implementation.  This may change in the
future.  It should run on any POSIX system supported by SBCL.

λleph communicates with clients via a RESTful API.  Some URLs of interest:

  * /feeds
  * /feeds/{id}
  * /feeds/{id}?format=raw
  * /feeds/{id}/items
  * /feeds/{id}/update
  * /feeds/{id}/mark-read
  * /items
  * /items/{id}
  * /items/{id}?format=raw
  * /items/{id}/mark-read

The server responds to requests by sending back JSON data representing the
requested resource.

λleph's policy on metadata is to store everything, even if it doesn't
understand what the data means.  This allows clients to be as smart or as
dumb as they want to be when presenting the data to the user.

By default, only the metadata that is meaningful to λleph is sent to clients.
In order to access the full range of feed or item metadata, clients can add
"format=raw" to the query string when requesting a feed or item resource.

Requirements
------------

λleph requires PostgreSQL and a few Common Lisp libraries.  Quicklisp is used
to install/load the required CL libraries.  You will have to install and
configure PostgreSQL yourself.

Configuration
-------------

λleph reads configuration settings from the file "rc.lisp" in the project's
root directory.  Settings are organized as a hierarchy of association lists.
E.g.

    ((:database
       (:name . "mydb")
       (:user . "me")
       (:pass . "mypass")
       (:host . "127.0.0.1"))
     (:server
       (:port . 80)
       (:document-root "www/")))

You will have to at least configure the database settings, unless you've
opted to use the default name/user/pass of "aleph" when setting up Postgres.
The default port for the HTTP server is 8080.

Running
-------

    $ sbcl --load run.lisp

Git Repository
--------------

https://github.com/drewt/aleph

    $ git clone https://github.com/drewt/aleph.git
