select case when parent is null then text2ltree('')
                       else f_pytis_help_page_position(parent)
                  end || to_char(coalesce(ord, 999999), 'FM000000')::text as result
           from e_pytis_help_pages where page_id=$1
