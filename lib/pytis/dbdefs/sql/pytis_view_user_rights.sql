select rights.shortname, rights.rights, rights.columns
from pytis_compute_summary_rights(NULL, pytis_user(), 'f', 'f', 'f') as rights;
