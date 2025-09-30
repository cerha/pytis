delete from e_pytis_action_rights where status<0;
update e_pytis_action_rights set status=0 where status>0;
