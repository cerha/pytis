update e_pytis_action_rights set status=-1 where shortname=$2;
insert into e_pytis_action_rights (shortname, roleid, rightid, colname, system, granted)
       select $2 as shortname, roleid, rightid, colname, system, granted from e_pytis_action_rights
              where shortname=$1 and status>=0;
