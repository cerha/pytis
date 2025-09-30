with newid as (
select '_user_profile_' || (coalesce(max(split_part(profile_id, '_',4)::int),0) + 1)::text as profile_id
   from ev_pytis_form_profiles p
   where p.username = $2
   and p.spec_name = (select spec_name from ev_pytis_form_profiles where id = $1 limit 1)
   ), profiles as
  (insert into e_pytis_form_profile_base
   (username, spec_name, profile_id, title, filter)
    select $2 as username, spec_name, newid.profile_id, title, filter
     from ev_pytis_form_profiles profiles, newid
    where id = $1 returning *), params as
     (insert into e_pytis_form_profile_params
      (username, spec_name, profile_id, form_name, params)
       select profiles.username, profiles.spec_name, newid.profile_id, params.form_name,
              params.params
         from ev_pytis_form_profiles params, newid, profiles
        where params.id = $1 returning *)
select profiles.id || '.' || params.id from profiles, params
