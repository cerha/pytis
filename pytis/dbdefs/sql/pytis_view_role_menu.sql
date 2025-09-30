select menu.menuid, menu.title, menu.position, menu.position_nsub, summary.roleid, summary.rights, 
       strpos(summary.rights, 'show')::bool as rights_show,
       strpos(summary.rights, 'view')::bool as rights_view,
       strpos(summary.rights, 'insert')::bool as rights_insert,
       strpos(summary.rights, 'update')::bool as rights_update,
       strpos(summary.rights, 'delete')::bool as rights_delete,
       strpos(summary.rights, 'print')::bool as rights_print,
       strpos(summary.rights, 'export')::bool as rights_export,
       strpos(summary.rights, 'call')::bool as rights_call
       from ev_pytis_menu as menu inner join pytis_compute_summary_rights(NULL, $1, $2, 't', 't') as summary
            on menu.shortname = summary.shortname;
