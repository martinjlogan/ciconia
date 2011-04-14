%% This is the application resource file (.app file) for the erlp,
%% application.
{application, faxien_driver,
 [{description, "A driver for a webdav faxien style repo."},
  {vsn, "0.5.0"},
  {modules, [
             faxien_driver,
             fd_repo_dav,
             fd_put
            ]},
  {registered,[]},
  {applications, [kernel, stdlib, erlpl, ewlib, erlware_commons, xmerl, ibrowse, ewrepo]},
  {start_phases, []}]}.

