Driver Creation Guide
=====================

Ciconia is designed to be used with multiple back end repo types. The communication between Ciconia and the backend repos is handled by a driver layer. Not all backend repos can be used with Ciconia however. It must be possible to map a back end repo so that the information about the packages contained within can be cached locally. It must also be possible to reference a backend package uniquely. Ciconia will search its local cache, find an acceptable package, and then pull it down directly when prompted to install the given package. 

To write a driver for Ciconia it must be created as a separate erlang application. That application and the file that implementes the driver interface should be named the same thing. For example, if I am creating a driver for interfacing with a couchdb based backend repo I might name the driver application `couchdb_driver`. In that case the source directory must contain a file called `couchdb_driver.erl`. This file is the one that will implement the Ciconia driver interface. `couchdb` would be the repo type in this case. If I were to want to update the Ciconia local cache from a repo of this type I would need to use something similar to the following example:

        ciconia update-cache -a couchdb -r http://mycouchdbrepo.com/repo

Notice of course the use of the `-a` option to specify the repo type. With that understood you can move on to understanding the Ciconia driver interface.

Ciconia Driver Interface
========================

The Ciconia driver interface consists of three functions.

       list/2
       get/2
       put/4
       
The list function is how the local cache is updated. This function only takes two arguments. A list of repos to list out and a timeout value for how long to take with each repo before failing. The timeout value can be ignored by the implementer of the driver. All URLs must succeed however or a ?UEX exception with an explanation of the falure must be returned. The UEX exception formatting macro can be found in the erlpl.hrl file. In fact, any failure within a driver should be signaled by throwing a UEX exception. To throw a UEX exception you must first `-include("erlpl.hrl").` and then throw the actual exception in a similar manner to the following example:
      
      Exception = {eaccess, FileName},
      Msg = "The following file ~p could not be accessed.~nPlease check that you have permissions~n",
      Vars = [FileName],
      throw(?UEX(Exception, Msg, Vars)

list/2
------

The list function must return a list of `#package_info` records. These records must have the following feilds populated

	#package_info{name = <package-name::string()>,
		      vsn = <package-vsn::string()>,
		      executable_env = "erts-<vsn>" | undefined,
		      repo_type = <repo-type::atom()>,
		      package_type = erts | release | app_binary_generic | app_binary_specific,
		      path = <specific-package-url::string()> | undefined,
		      repo = <repo-url::string()>
		      };


The package_type field holds one of 4 atoms. One of these is `app_binary_specific`. This indicates that some part of the application package has been natively compiled and will only function on a compatible system. It is the job of the driver writer to only pull down packages that are compatible with the OS that Ciconia is running on. There are a few helper functions within Ciconia to help the driver writer with this. They are contained in the `erlpl` application within the `epl_os_specific` module. The specific functions are:

        hardware_name/0,
        os_release/0,
	glibc_version/0,
	os_name/0

The only package type that should have the executable_env field marked as undefined is the erts package. `undefined` in that case signifies that erts itself does not run in a VM, which makes sense because it is a VM. Releases and apps run within erts therefore their executable environments are ERTS at some version. In the case of our example above repo_type would be `couchdb`. 

get/2
-----

This function takes a package info record as filled in by the list function and a timeout value. This timeout value should not be ignored. This function uses the information in the package info record to fetch the remote package. Most of the time this involves using the `path` url directly to fetch the package. If the repo type does not support direct URL based access to packages the information needed to fetch the package should have been stored in the `meta` feild of the package_info record. The meta feild is can be whatever the driver writer wants it to be. The get/2 function needs to return a the binary data for the package requested. With package fetching out of the way we can move into how it is possible to put a package into a repo.

put/4
-----

This function is created to assist in publishing packages. Ciconia supplies 4 things and a lot of information to this function. The first are the repos that this package should be placed into. The second thing is a path to the package itself. Third is a populated `package_info` record for the package. As usual the timeout value, which should be respected, is passed in. From there, it is the driver writers job to push the package to the repo in a manner that cooresponds with the package itself and will allow for the list and get functions to find it again.
