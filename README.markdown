Package Management with ciconia
============================

*Ciconia* offers the user the most comprehensive and powerful package management
functionality available for Erlang software. 
The first part of this text is a short tutorial indicating what a new user
might do first upon installing ciconia. The first step is to update our
local repo cache.

	ciconia update-cache -r http://beerenthusiasts.cloudant.com

This command updates the local cache of packages from the repo specified 
after the -r flag. This will allow us to search and install packages
from the repo specified. More than one repo can be added by separating
repos by commas (but no spaces <repo1>,<repo2>). Next lets put any 
existing root directories under ciconia management.

	ciclocal manage-root-dir -d /usr/local/lib/erlang
	ciclocal dump-db

The first command takes our existing erlang root dir and puts it under
management. The second command shows us a listing of all the packages
we now have under management. Notice the use of ciclocal instead of 
ciconia. This is becuase ciclocal is for local package managment functions
while ciconia exists to operate with repos. Now we will do just that;
interact with a repo.

	ciconia install-app -f -d /usr/local/lib/erlang mochiweb

You have just installed the application mochiweb into our root dir
at `/usr/local/lib/erlang`. You can make sure it is there using
the `ciclocal dump-db | grep mochiweb`. Notice if you will the use 
of the `-f` flag. This indicates that no prompting of the user 
should be done, just install it regardless of situation.
Next you can go ahead and publish one of your own applications 
to the remote repo.

	ciconia publish -vr http://beerenthusiasts.cloudant.com _build/development/apps/ciclocal-0.2.0

Notice the use of the -v option. This indicates verbose output so
that you can debug if something goes wrong. For more info type

	ciconia help

Using Ciconia in place of Faxien
=============================

Ciconia is fully backwards compatible with Faxien formatted 
Erlware webdav based repos. To use Ciconia in this context you can:

	ciconia update-cache -a faxien -vr http://repo.erlware.org/pub
	ciconia install-app -d /usr/local/erlware erlcron

The first command cached the repo data. Notice the use of the `-a` 
flag. This signals to ciconia the repo type it is dealing with. In
this case a faxien type. Moving on to the second command we see
the erlcron app installed in to the root dir at `/usr/local/erlware`.

Installing a release looks like this: 

	ciconia install-release -d /usr/local/erlware portius

Publishing is the next topic. The following line would be used
to publish the ciconia release to a faxien repo on the local file
system.

        ciconia publish -va faxien -r file://./tmp/repo -R ./_build/development/tar/ciconia-0.2.0.tar.gz

Notice the use of the -R flag. This causes ciconia to publish the 
directory recursively. With this flag you could publish an 
entire lib dir. Or publish all the applications and the erts 
package that sit in a release package. Generically speaking we have:

        ciconia publish -a <repo-type> -r <repo-url> <path-to-package>

Using Ciclocal in place of Faxien
==============================

Ciclocal is the local package manager. To get started using this in
place of faxien try building a package. If using Sinan run

        sinan dist
	
then install the created package like this:

        ciclocal install-release -e /usr/local/erlware/erts-<vsn> -d ./tmp/erlware _build/development/tar/<tarball>
	
For example:

        ciclocal install-release -e /usr/local/erlware/erts-5.8.2 -d tmp/erlware -f _build/development/tar/ciconia-0.2.0.tar.gz

This just installed the ciconia release into a directory at `tmp/erlware`
an erts package found at `/usr/local/erlware/erts-5.8.2`.


Architecture
============

ciconia consists of multiple parts. The first we will talk about is
ciclocal.

ciclocal - local package management
--------------------------------

 ciclocal is the local package management system. ciconial does 
not know a thing about remote packages. It can handle archives
or directories that are valid packages. It can install,
uninstall, rollback, list and generally manage these types of
packages on the local file system. Getting these packages
on the local file system is primarily the job of ciconia which
we discuss breifly next.

ciconia - remote package management
--------------------------------

ciconia pulls and pushes packages back and forth from the local
file system to remote repositories. ciconia caches remote 
repository information locally and uses that meta data to
install packages. Ciconia is capable of installing Erlang/OTP
applications, releases, and erts packages. In order to 
interact with various repo types such as couchdb, webdav,
and agner repos ciconia relies on repository drivers.

Repository Drivers
------------------

Repository drivers define an interface and are used by
ciconia to interact with various repo types. These drivers
implement the following functions

- get
- put
- list

Through these functions and the `#package_info` record
information is passed between ciconia and the various repo
types it is compatible with. 
