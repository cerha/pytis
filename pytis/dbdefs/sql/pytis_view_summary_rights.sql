select summary.shortname, summary.roleid, summary.rights, summary.columns,
       purposes.purpose,
       strpos(summary.rights, 'show')::bool as rights_show,
       strpos(summary.rights, 'view')::bool as rights_view,
       strpos(summary.rights, 'insert')::bool as rights_insert,
       strpos(summary.rights, 'update')::bool as rights_update,
       strpos(summary.rights, 'delete')::bool as rights_delete,
       strpos(summary.rights, 'print')::bool as rights_print,
       strpos(summary.rights, 'export')::bool as rights_export,
       strpos(summary.rights, 'call')::bool as rights_call
       from pytis_compute_summary_rights($1, $2, $3, $4, 'f') as summary
            left outer join e_pytis_roles as roles on summary.roleid = roles.name
            left outer join c_pytis_role_purposes as purposes on roles.purposeid = purposes.purposeid;
