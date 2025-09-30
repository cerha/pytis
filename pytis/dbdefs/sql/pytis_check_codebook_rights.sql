(
 select roleid from pytis_compute_summary_rights('form/'||$1, NULL, $4, 'f', 'f') where (rights like '%insert%' or rights like '%update%') and (' '||columns||' ') like '% $2 %'
 union
 (
  select roleid from pytis_compute_summary_rights('form/'||$1, NULL, $4, 'f', 'f') where (rights like '%insert%' or rights like '%update%') and columns=''
  except
  select roleid from pytis_compute_summary_rights('form/'||$1, NULL, $4, 'f', 'f') where (' '||columns||' ') like '% $2 %'
 )
)
intersect
select roleid from pytis_compute_summary_rights('form/'||$3, NULL, $4, 'f', 't') where rights not like '%view%';
