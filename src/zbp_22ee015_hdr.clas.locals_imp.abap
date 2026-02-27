CLASS lhc_SalesOrderHdr DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR SalesOrderHdr RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR SalesOrderHdr RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE SalesOrderHdr.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE SalesOrderHdr.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE SalesOrderHdr.

    METHODS read FOR READ
      IMPORTING keys FOR READ SalesOrderHdr RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK SalesOrderHdr.

    METHODS rba_Salesitem FOR READ
      IMPORTING keys_rba FOR READ SalesOrderHdr\_Salesitem FULL result_requested RESULT result LINK association_links.

    METHODS cba_Salesitem FOR MODIFY
      IMPORTING entities_cba FOR CREATE SalesOrderHdr\_Salesitem.

ENDCLASS.

CLASS lhc_SalesOrderHdr IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD create.
    DATA: ls_sales_hdr TYPE zmk_header_t.

    LOOP AT entities INTO DATA(ls_entities).
      ls_sales_hdr = CORRESPONDING #( ls_entities MAPPING FROM ENTITY ).

      IF ls_sales_hdr-salesdocument IS NOT INITIAL.

        SELECT SINGLE FROM zmk_header_t FIELDS salesdocument
          WHERE salesdocument = @ls_sales_hdr-salesdocument
          INTO @DATA(lv_exist).

        IF sy-subrc NE 0.
          "Record does not exist - safe to create
          DATA(lo_util) = zcl_22ee015_util=>get_instance( ).
          lo_util->set_hdr_value(
            EXPORTING im_sales_hdr = ls_sales_hdr
            IMPORTING ex_created   = DATA(lv_created) ).

          IF lv_created EQ abap_true.
            APPEND VALUE #(
              %cid          = ls_entities-%cid
              salesdocument = ls_sales_hdr-salesdocument )
            TO mapped-salesorderhdr.

            APPEND VALUE #(
              %cid          = ls_entities-%cid
              salesdocument = ls_sales_hdr-salesdocument
              %msg          = new_message(
                                id       = 'ZMK_SALES_MSG'
                                number   = '001'
                                v1       = 'Sales Order Created Successfully'
                                severity = if_abap_behv_message=>severity-success ) )
            TO reported-salesorderhdr.
          ENDIF.

        ELSE.
          "Duplicate record found
          APPEND VALUE #(
            %cid          = ls_entities-%cid
            salesdocument = ls_sales_hdr-salesdocument )
          TO failed-salesorderhdr.

          APPEND VALUE #(
            %cid          = ls_entities-%cid
            salesdocument = ls_sales_hdr-salesdocument
            %msg          = new_message(
                              id       = 'ZMK_SALES_MSG'
                              number   = '001'
                              v1       = 'Duplicate Sales Order Found'
                              severity = if_abap_behv_message=>severity-error ) )
          TO reported-salesorderhdr.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD update.
    DATA: ls_sales_hdr TYPE zmk_header_t.

    LOOP AT entities INTO DATA(ls_entities).
      ls_sales_hdr = CORRESPONDING #( ls_entities MAPPING FROM ENTITY ).

      IF ls_sales_hdr-salesdocument IS NOT INITIAL.

        SELECT SINGLE FROM zmk_header_t FIELDS salesdocument
          WHERE salesdocument = @ls_sales_hdr-salesdocument
          INTO @DATA(lv_exist).

        IF sy-subrc EQ 0.
          "Record exists - update buffer
          DATA(lo_util) = zcl_22ee015_util=>get_instance( ).
          lo_util->set_hdr_value(
            EXPORTING im_sales_hdr = ls_sales_hdr
            IMPORTING ex_created   = DATA(lv_created) ).

          IF lv_created EQ abap_true.
            APPEND VALUE #(
              salesdocument = ls_sales_hdr-salesdocument )
            TO mapped-salesorderhdr.

            APPEND VALUE #(
              %key = ls_entities-%key
              %msg = new_message(
                       id       = 'ZMK_SALES_MSG'
                       number   = '001'
                       v1       = 'Sales Order Updated Successfully'
                       severity = if_abap_behv_message=>severity-success ) )
            TO reported-salesorderhdr.
          ENDIF.

        ELSE.
          "Record not found
          APPEND VALUE #(
            %cid          = ls_entities-%cid_ref
            salesdocument = ls_sales_hdr-salesdocument )
          TO failed-salesorderhdr.

          APPEND VALUE #(
            %cid          = ls_entities-%cid_ref
            salesdocument = ls_sales_hdr-salesdocument
            %msg          = new_message(
                              id       = 'ZMK_SALES_MSG'
                              number   = '001'
                              v1       = 'Sales Order Not Found'
                              severity = if_abap_behv_message=>severity-error ) )
          TO reported-salesorderhdr.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
    DATA(lo_util) = zcl_22ee015_util=>get_instance( ).

    LOOP AT keys INTO DATA(ls_key).

      SELECT SINGLE FROM zmk_header_t FIELDS salesdocument
        WHERE salesdocument = @ls_key-salesdocument
        INTO @DATA(lv_exist).

      IF sy-subrc EQ 0.
        "Record exists - mark for deletion
        lo_util->set_hdr_t_deletion(
          im_sales_doc = VALUE #(
            salesdocument = ls_key-salesdocument ) ).
        lo_util->set_hdr_deletion_flag(
          im_so_delete = abap_true ).

        APPEND VALUE #(
          %cid          = ls_key-%cid_ref
          salesdocument = ls_key-salesdocument
          %msg          = new_message(
                            id       = 'ZMK_SALES_MSG'
                            number   = '001'
                            v1       = 'Sales Order Deleted Successfully'
                            severity = if_abap_behv_message=>severity-success ) )
        TO reported-salesorderhdr.

      ELSE.
        "Record not found
        APPEND VALUE #(
          %cid          = ls_key-%cid_ref
          salesdocument = ls_key-salesdocument )
        TO failed-salesorderhdr.

        APPEND VALUE #(
          %cid          = ls_key-%cid_ref
          salesdocument = ls_key-salesdocument
          %msg          = new_message(
                            id       = 'ZMK_SALES_MSG'
                            number   = '001'
                            v1       = 'Sales Order Not Found for Deletion'
                            severity = if_abap_behv_message=>severity-error ) )
        TO reported-salesorderhdr.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD read.
    LOOP AT keys INTO DATA(ls_key).

      SELECT SINGLE FROM zmk_header_t FIELDS *
        WHERE salesdocument = @ls_key-salesdocument
        INTO @DATA(ls_hdr).

      IF sy-subrc EQ 0.
        APPEND CORRESPONDING #( ls_hdr ) TO result.
      ELSE.
        APPEND VALUE #(
          salesdocument = ls_key-salesdocument
          %fail-cause   = if_abap_behv=>cause-not_found )
        TO failed-salesorderhdr.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD rba_Salesitem.
    LOOP AT keys_rba INTO DATA(ls_key).

      SELECT FROM zmk_item_t FIELDS *
        WHERE salesdocument = @ls_key-salesdocument
        INTO TABLE @DATA(lt_items).

      LOOP AT lt_items INTO DATA(ls_item).
        APPEND CORRESPONDING #( ls_item ) TO result.
        APPEND VALUE #(
          source-salesdocument   = ls_key-salesdocument
          target-salesdocument   = ls_item-salesdocument
          target-salesitemnumber = ls_item-salesitemnumber )
        TO association_links.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD cba_Salesitem.
    DATA: ls_sales_itm TYPE zmk_item_t.
    DATA(lo_util) = zcl_22ee015_util=>get_instance( ).

    LOOP AT entities_cba INTO DATA(ls_entities_cba).

      "Loop over %target directly - fixes structure unknown error
      LOOP AT ls_entities_cba-%target INTO DATA(ls_target).

        ls_sales_itm = CORRESPONDING #( ls_target MAPPING FROM ENTITY ).

        IF ls_sales_itm-salesdocument   IS NOT INITIAL AND
           ls_sales_itm-salesitemnumber IS NOT INITIAL.

          SELECT SINGLE FROM zmk_item_t FIELDS salesdocument
            WHERE salesdocument   = @ls_sales_itm-salesdocument
            AND   salesitemnumber = @ls_sales_itm-salesitemnumber
            INTO @DATA(lv_exist).

          IF sy-subrc NE 0.
            "Item does not exist - safe to create
            lo_util->set_itm_value(
              EXPORTING im_sales_itm = ls_sales_itm
              IMPORTING ex_created   = DATA(lv_created) ).

            IF lv_created EQ abap_true.
              APPEND VALUE #(
                %cid            = ls_target-%cid
                salesdocument   = ls_sales_itm-salesdocument
                salesitemnumber = ls_sales_itm-salesitemnumber )
              TO mapped-salesorderitm.

              APPEND VALUE #(
                %cid          = ls_target-%cid
                salesdocument = ls_sales_itm-salesdocument
                %msg          = new_message(
                                  id       = 'ZMK_SALES_MSG'
                                  number   = '001'
                                  v1       = 'Sales Item Created Successfully'
                                  severity = if_abap_behv_message=>severity-success ) )
              TO reported-salesorderitm.
            ENDIF.

          ELSE.
            "Duplicate item found
            APPEND VALUE #(
              %cid            = ls_target-%cid
              salesdocument   = ls_sales_itm-salesdocument
              salesitemnumber = ls_sales_itm-salesitemnumber )
            TO failed-salesorderitm.

            APPEND VALUE #(
              %cid          = ls_target-%cid
              salesdocument = ls_sales_itm-salesdocument
              %msg          = new_message(
                                id       = 'ZMK_SALES_MSG'
                                number   = '002'
                                v1       = 'Duplicate Sales Item Found'
                                severity = if_abap_behv_message=>severity-error ) )
            TO reported-salesorderitm.
          ENDIF.

        ENDIF.
      ENDLOOP. "end %target loop

    ENDLOOP. "end entities_cba loop
  ENDMETHOD.

ENDCLASS.
