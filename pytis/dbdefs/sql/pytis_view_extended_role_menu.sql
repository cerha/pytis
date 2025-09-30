select structure.shortname, structure.position, structure.type,
       menu.menuid, menu.next_position, menu.help, menu.hotkey, menu.locked,
       atypes.description as actiontype,
       (select count(*)-1 from a_pytis_actions_structure where position <@ structure.position) as position_nsub,
       coalesce(menu.title, '('||actions.action_title||')') as title,
       structure.fullname, summary.roleid,
       summary.rights,
       strpos(summary.rights, 'show')::bool as rights_show,
       strpos(summary.rights, 'view')::bool as rights_view,
       strpos(summary.rights, 'insert')::bool as rights_insert,
       strpos(summary.rights, 'update')::bool as rights_update,
       strpos(summary.rights, 'delete')::bool as rights_delete,
       strpos(summary.rights, 'print')::bool as rights_print,
       strpos(summary.rights, 'export')::bool as rights_export,
       strpos(summary.rights, 'call')::bool as rights_call
from a_pytis_actions_structure as structure
     left outer join e_pytis_menu as menu on (structure.menuid = menu.menuid)
     inner join pytis_compute_summary_rights(NULL, $1, $2, 't', 't') as summary on (structure.shortname = summary.shortname)
     left outer join c_pytis_action_types as atypes on (structure.type = atypes.type)
     left outer join c_pytis_menu_actions as actions on (structure.fullname = actions.fullname)
     where pytis_multiform_spec(structure.fullname)
union
select structure.shortname, structure.position, structure.type,
       menu.menuid, menu.next_position, menu.help, menu.hotkey, menu.locked,
       atypes.description as actiontype,
       (select count(*)-1 from a_pytis_actions_structure where position <@ structure.position) as position_nsub,
       coalesce(menu.title, '('||actions.action_title||')') as title,
       structure.fullname, summary.roleid,
       summary.rights,
       strpos(summary.rights, 'show')::bool as rights_show,
       strpos(summary.rights, 'view')::bool as rights_view,
       strpos(summary.rights, 'insert')::bool as rights_insert,
       strpos(summary.rights, 'update')::bool as rights_update,
       strpos(summary.rights, 'delete')::bool as rights_delete,
       strpos(summary.rights, 'print')::bool as rights_print,
       strpos(summary.rights, 'export')::bool as rights_export,
       strpos(summary.rights, 'call')::bool as rights_call
from a_pytis_actions_structure as structure
     left outer join e_pytis_menu as menu on (structure.menuid = menu.menuid)
     inner join pytis_compute_summary_rights(NULL, $1, $2, 'f', 't') as summary on (structure.shortname = summary.shortname)
     left outer join c_pytis_action_types as atypes on (structure.type = atypes.type)
     left outer join c_pytis_menu_actions as actions on (structure.fullname = actions.fullname)
     where not pytis_multiform_spec(structure.fullname);
