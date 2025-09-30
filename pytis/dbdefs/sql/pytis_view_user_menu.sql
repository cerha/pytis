select menu.menuid, menu.name, coalesce(menu.t_title, menu.title), menu.position, menu.next_position, menu.fullname,
       menu.help, menu.hotkey, menu.locked, menu.language
from ev_pytis_translated_menu as menu
left outer join pytis_compute_summary_rights(NULL, pytis_user(), 'f', 't', 't') as rights on (menu.shortname = rights.shortname)
where pytis_multiform_spec(menu.fullname) and rights.rights like '%show%'
union
select menu.menuid, menu.name, coalesce(menu.t_title, menu.title), menu.position, menu.next_position, menu.fullname,
       menu.help, menu.hotkey, menu.locked, menu.language
from ev_pytis_translated_menu as menu
left outer join pytis_compute_summary_rights(NULL, pytis_user(), 'f', 'f', 't') as rights on (menu.shortname = rights.shortname)
where not pytis_multiform_spec(menu.fullname) and rights.rights like '%show%'
union
select menu.menuid, menu.name, coalesce(menu.t_title, menu.title), menu.position, menu.next_position, menu.fullname,
       menu.help, menu.hotkey, menu.locked, menu.language
from ev_pytis_translated_menu as menu
where name is null and title is null;
