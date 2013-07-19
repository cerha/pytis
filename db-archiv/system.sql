-- Pytis system tables

create schema pytis;

create table pytis.access_rights (id serial,
                                  object varchar(32),
                                  column_ varchar(32),
                                  group_ name,
                                  permission varchar(32));
create index access_rights_object on pytis.access_rights (object);

-- Local Variables:
-- sql-product: postgres
-- End:
