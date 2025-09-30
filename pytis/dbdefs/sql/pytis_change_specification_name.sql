insert into e_pytis_disabled_dmp_triggers (id) values ('genmenu');
update c_pytis_menu_actions set fullname=pytis_change_fullname(fullname, $1, $2), shortname=pytis_change_shortname(shortname, $1, $2) where shortname != pytis_change_shortname(shortname, $1, $2) or fullname != pytis_change_fullname(fullname, $1, $2);
update e_pytis_menu set fullname=pytis_change_fullname(fullname, $1, $2) where fullname != pytis_change_fullname(fullname, $1, $2);
update e_pytis_action_rights set shortname=pytis_change_shortname(shortname, $1, $2) where shortname != pytis_change_shortname(shortname, $1, $2);
update a_pytis_actions_structure set fullname=pytis_change_fullname(fullname, $1, $2), shortname=pytis_change_shortname(shortname, $1, $2) where shortname != pytis_change_shortname(shortname, $1, $2) or fullname != pytis_change_fullname(fullname, $1, $2);
delete from e_pytis_disabled_dmp_triggers where id='genmenu';
