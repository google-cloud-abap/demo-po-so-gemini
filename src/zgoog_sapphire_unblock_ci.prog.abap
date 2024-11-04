*&---------------------------------------------------------------------*
*& Include          ZGOOG_SAPPHIRE_UNBLOCK_CI
*&---------------------------------------------------------------------*

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.

    TRY.
        CREATE OBJECT mo_pubsub_client
          EXPORTING
            iv_key_name = p_key.
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        DATA(lv_msg) = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD execute.

    CALL METHOD pull_so_unblock_request
      IMPORTING
        ex_error              = DATA(lv_error)
        et_so_unblock_request = DATA(lt_so_unblock_request).
    IF lv_error IS NOT INITIAL.
      MESSAGE lv_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF lt_so_unblock_request IS NOT INITIAL.
      CALL METHOD unblock_sales_order
        EXPORTING
          it_so_unblock_request = lt_so_unblock_request
        IMPORTING
          et_output             = DATA(lt_output).
      IF lt_output IS NOT INITIAL.
        CALL METHOD display_output
          EXPORTING
            im_t_output = lt_output.

      ENDIF.
    ELSE.
      MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ENDMETHOD.

  METHOD pull_so_unblock_request.

    DATA:
      ls_input              TYPE /goog/cl_pubsub_v1=>ty_026,
      ls_input_ack          TYPE /goog/cl_pubsub_v1=>ty_001,
      ls_message            TYPE /goog/cl_pubsub_v1=>ty_029,
      ls_so_unblock_request TYPE gty_so_unblock_request.

    TRY.
        ls_input-max_messages = 50.

* Call API method
        CALL METHOD mo_pubsub_client->pull_subscriptions
          EXPORTING
            iv_p_projects_id      = CONV #( mo_pubsub_client->gv_project_id )
            iv_p_subscriptions_id = p_sub_id
            is_input              = ls_input
          IMPORTING
*           es_raw                =
            es_output             = DATA(ls_output)
            ev_ret_code           = DATA(lv_ret_code)
            ev_err_text           = DATA(lv_err_text)
            es_err_resp           = DATA(ls_err_resp).
        IF mo_pubsub_client->is_success( lv_ret_code ).
          IF ls_output-received_messages IS NOT INITIAL.
            ls_input_ack-ack_ids = VALUE #( FOR ls_msg IN ls_output-received_messages ( ls_msg-ack_id ) ).
            CALL METHOD mo_pubsub_client->acknowledge_subscriptions
              EXPORTING
                iv_p_projects_id      = CONV #( mo_pubsub_client->gv_project_id )
                iv_p_subscriptions_id = p_sub_id
                is_input              = ls_input_ack
              IMPORTING
                ev_ret_code           = lv_ret_code
                ev_err_text           = lv_err_text
                es_err_resp           = ls_err_resp.
            LOOP AT ls_output-received_messages INTO ls_message.
              DATA(lv_msg) = cl_http_utility=>decode_base64( encoded = ls_message-message-data ).
              /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_msg
                                                              iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                                    IMPORTING es_data        = ls_so_unblock_request ).

              APPEND ls_so_unblock_request TO et_so_unblock_request.

            ENDLOOP.

          ENDIF.
        ELSE.
          ex_error = lv_err_text.

        ENDIF.

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        MESSAGE lo_exception->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD unblock_sales_order.

    DATA:
      ls_order_header_in  TYPE bapisdh1,
      ls_order_header_inx TYPE bapisdh1x,
      lt_return           TYPE STANDARD TABLE OF bapiret2,
      lv_vbeln            TYPE vbeln_va,
      ls_output           TYPE gty_output.

    LOOP AT it_so_unblock_request ASSIGNING FIELD-SYMBOL(<ls_so_unblock_request>).
      lv_vbeln = CONV #( <ls_so_unblock_request>-sales_order ).

      ls_order_header_inx-updateflag = 'U'.
      ls_order_header_inx-dlv_block  = 'X'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = lv_vbeln
          order_header_in  = ls_order_header_in
          order_header_inx = ls_order_header_inx
        TABLES
          return           = lt_return.
      IF lt_return IS NOT INITIAL.
        ls_output-sales_order    = <ls_so_unblock_request>-sales_order.
        ls_output-purchase_order = <ls_so_unblock_request>-purchase_order.

        READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>)
          WITH KEY type = 'E'.
        IF sy-subrc = 0.
          ls_output-status = text-004 && <ls_return>-message.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          ls_output-status = text-003.

        ENDIF.

        APPEND ls_output TO et_output.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD display_output.

    DATA:
          lo_column     TYPE REF TO cl_salv_column_table.

    DATA(lt_output) = im_t_output.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = lt_output.
      CATCH cx_salv_msg.
    ENDTRY.

    DATA(lo_functions) = lo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

    DATA(lo_columns) = lo_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).

    TRY.
        lo_column ?= lo_columns->get_column( 'SALES_ORDER' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Sales Order' ).
        lo_column->set_medium_text( 'Sales Order' ).
        lo_column->set_short_text( 'SO' ).

        lo_column ?= lo_columns->get_column( 'PURCHASE_ORDER' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Purchase Order' ).
        lo_column->set_medium_text( 'Purchase Order' ).
        lo_column->set_short_text( 'PO' ).

        lo_column ?= lo_columns->get_column( 'REMARKS' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Remarks' ).
        lo_column->set_medium_text( 'Remarks' ).
        lo_column->set_short_text( 'Remarks' ).

      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    lo_alv->display( ).

  ENDMETHOD.

  METHOD close_connection.

    mo_pubsub_client->close( ).

  ENDMETHOD.

ENDCLASS.
