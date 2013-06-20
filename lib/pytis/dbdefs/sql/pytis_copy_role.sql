delete from e_pytis_role_members
       where member=$2 and roleid in (select roleid from e_pytis_roles where purposeid='appl');
insert into e_pytis_role_members (roleid, member)
       select roleid, $2 as member from e_pytis_role_members left join e_pytis_roles on roleid=name
              where member=$1 and purposeid='appl';
