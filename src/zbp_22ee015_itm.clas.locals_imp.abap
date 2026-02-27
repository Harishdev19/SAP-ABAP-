CLASS lhc_SalesOrderItm DEFINITION
  INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE SalesOrderItm.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE SalesOrderItm.

    METHODS read FOR READ
      IMPORTING keys FOR READ SalesOrderItm
      RESULT result.

    METHODS rba_SalesHeader FOR READ
      IMPORTING keys_rba FOR READ
      SalesOrderItm\_SalesHeader
      FULL result_requested
      RESULT result
      LINK association_links.

ENDCLASS.

CLASS lhc_SalesOrderItm IMPLEMENTATION.

  METHOD update.
    DATA ls_sales_itm TYPE zmk_item_t.

    LOOP AT entities INTO DATA(ls_entities).
      ls_sales_itm = CORRESPONDING #(
        ls_entities MAPPING FROM ENTITY ).

      IF ls_sales_itm-salesdocument IS NOT INITIAL.

        SELECT SINGLE FROM zmk_item_t
          FIELDS salesdocument
          WHERE salesdocument   = @ls_sales_itm-salesdocument
          AND   salesitemnumber = @ls_sales_itm-salesitemnumber
          INTO @DATA(lv_exist).

        IF sy-subrc EQ 0.
          DATA(lo_util) = zcl_22ee015_util=>get_instance( ).
          lo_util->set_itm_value(
            EXPORTING im_sales_itm = ls_sales_itm
            IMPORTING ex_created   = DATA(lv_created) ).

          IF lv_created EQ abap_true.
            APPEND VALUE #(
              salesdocument   = ls_sales_itm-salesdocument
              salesitemnumber = ls_sales_itm-salesitemnumber )
            TO mapped-salesorderitm.

            APPEND VALUE #(
              %key = ls_entities-%key
              %msg = new_message(
                       id       = 'ZMK_SALES_MSG'
                       number   = '001'
                       v1       = 'Sales Item Updated Successfully'
                       severity = if_abap_behv_message=>severity-success ) )
            TO reported-salesorderitm.
          ENDIF.

        ELSE.
          APPEND VALUE #(
            %cid            = ls_entities-%cid_ref
            salesdocument   = ls_sales_itm-salesdocument
            salesitemnumber = ls_sales_itm-salesitemnumber )
          TO failed-salesorderitm.

          APPEND VALUE #(
            %cid          = ls_entities-%cid_ref
            salesdocument = ls_sales_itm-salesdocument
            %msg          = new_message(
                              id       = 'ZMK_SALES_MSG'
                              number   = '001'
                              v1       = 'Sales Item Not Found'
                              severity = if_abap_behv_message=>severity-error ) )
          TO reported-salesorderitm.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
    DATA(lo_util) = zcl_22ee015_util=>get_instance( ).

    LOOP AT keys INTO DATA(ls_key).

      SELECT SINGLE FROM zmk_item_t
        FIELDS salesdocument
        WHERE salesdocument   = @ls_key-salesdocument
        AND   salesitemnumber = @ls_key-salesitemnumber
        INTO @DATA(lv_exist).

      IF sy-subrc EQ 0.
        lo_util->set_itm_t_deletion(
          im_sales_itm_info = VALUE #(
            salesdocument   = ls_key-salesdocument
            salesitemnumber = ls_key-salesitemnumber ) ).

        APPEND VALUE #(
          %cid          = ls_key-%cid_ref
          salesdocument = ls_key-salesdocument
          %msg          = new_message(
                            id       = 'ZMK_SALES_MSG'
                            number   = '001'
                            v1       = 'Sales Item Deleted Successfully'
                            severity = if_abap_behv_message=>severity-success ) )
        TO reported-salesorderitm.

      ELSE.
        APPEND VALUE #(
          %cid            = ls_key-%cid_ref
          salesdocument   = ls_key-salesdocument
          salesitemnumber = ls_key-salesitemnumber )
        TO failed-salesorderitm.

        APPEND VALUE #(
          %cid          = ls_key-%cid_ref
          salesdocument = ls_key-salesdocument
          %msg          = new_message(
                            id       = 'ZMK_SALES_MSG'
                            number   = '001'
                            v1       = 'Sales Item Not Found for Deletion'
                            severity = if_abap_behv_message=>severity-error ) )
        TO reported-salesorderitm.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD read.
    LOOP AT keys INTO DATA(ls_key).

      SELECT SINGLE FROM zmk_item_t
        FIELDS *
        WHERE salesdocument   = @ls_key-salesdocument
        AND   salesitemnumber = @ls_key-salesitemnumber
        INTO @DATA(ls_itm).

      IF sy-subrc EQ 0.
        APPEND CORRESPONDING #( ls_itm ) TO result.
      ELSE.
        APPEND VALUE #(
          salesdocument   = ls_key-salesdocument
          salesitemnumber = ls_key-salesitemnumber
          %fail-cause     = if_abap_behv=>cause-not_found )
        TO failed-salesorderitm.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD rba_SalesHeader.
    LOOP AT keys_rba INTO DATA(ls_key).

      SELECT SINGLE FROM zmk_header_t
        FIELDS *
        WHERE salesdocument = @ls_key-salesdocument
        INTO @DATA(ls_hdr).

      IF sy-subrc EQ 0.
        APPEND CORRESPONDING #( ls_hdr ) TO result.
        APPEND VALUE #(
          source-salesdocument = ls_key-salesdocument
          target-salesdocument = ls_hdr-salesdocument )
        TO association_links.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
