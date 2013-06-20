select case when parent is null then text2ltree('')
                       else cms_menu_structure_tree_order(parent)
                  end || to_char(coalesce(ord, 999999), 'FM000000')::text as result
           from cms_menu_structure where menu_item_id=$1
