(
 select *, False as change from pytis_view_summary_rights($1, $2, 'f', $3)
 except
 select *, False as change from pytis_view_summary_rights($1, $2, 't', $3)
)
union
(
 select *, True as change from pytis_view_summary_rights($1, $2, 't', $3)
 except
 select *, True as change from pytis_view_summary_rights($1, $2, 'f', $3)
);
