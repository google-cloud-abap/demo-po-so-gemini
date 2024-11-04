*&---------------------------------------------------------------------*
*& Include          ZGOOG_PROCESS_PO_AUTO_DEF
*&---------------------------------------------------------------------*

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      execute,
      close_connection.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_po_content,
        name      TYPE string,
        mime_type TYPE string,
        content   TYPE xstring,

      END OF gty_po_content,

      BEGIN OF gty_documentitemdata,
        documentname           TYPE string,
        itemid                 TYPE /iwbep/sb_odata_ty_int2,
        customermaterialnumber TYPE string,
        quantity               TYPE p LENGTH 7 DECIMALS 3,

      END OF gty_documentitemdata,

      BEGIN OF gty_documentdata,
        documentname                   TYPE string,
        purchaseordernumber            TYPE string,
        purchaseorderdate              TYPE string,
        requesteddeliverydate          TYPE string,
        soldtonumber                   TYPE c LENGTH 10,
        soldtoname                     TYPE string,
        soldtoaddress                  TYPE string,
        shiptonumber                   TYPE c LENGTH 10,
        shiptoname                     TYPE string,
        shiptoaddress                  TYPE string,
        deliveryinstructions           TYPE string,
        deliveryinstructionsoriginal   TYPE string,
        deliveryinstructionsoriginalla TYPE string,
        salesordernumber               TYPE c LENGTH 10,
        deliveryblock                  TYPE lifsk,
        explanation                    TYPE string,
        recommendation                 TYPE string,
        documentdataitemset            TYPE TABLE OF gty_documentitemdata WITH DEFAULT KEY,

      END OF gty_documentdata,

      BEGIN OF gty_output,
        file_name TYPE string,
        ebeln     TYPE ebeln,
        vbeln     TYPE vbeln_va,
        remarks   TYPE string,

      END OF gty_output.

    TYPES:
      gty_t_po_content   TYPE STANDARD TABLE OF gty_po_content,
      gty_t_documentdata TYPE STANDARD TABLE OF gty_documentdata,
      gty_t_output       TYPE STANDARD TABLE OF gty_output.

    DATA:
      mo_storage_client   TYPE REF TO /goog/cl_storage_v1,
      mo_docai_client     TYPE REF TO /goog/cl_documentai_v1,
      mo_addrvaldn_client TYPE REF TO /goog/cl_addrvaldn_v1,
      mo_translate_client TYPE REF TO /goog/cl_translation_v2,
      mo_pubsub_client    TYPE REF TO /goog/cl_pubsub_v1,
      gt_t_objects        TYPE /goog/cl_storage_v1=>ty_t_013.

    METHODS:
      read_purchase_orders
        EXPORTING
          ex_t_po_content TYPE gty_t_po_content,
      move_purchase_order
        importing
          im_target type string,
      list_bucket_objects
        EXPORTING
          ex_t_objects TYPE /goog/cl_storage_v1=>ty_t_013,
      read_bucket_object
        IMPORTING
          iv_object_name TYPE string
        EXPORTING
          ex_mime_type   TYPE string
          es_content     TYPE xstring,
      process_purchase_orders
        IMPORTING
          im_t_po_content TYPE gty_t_po_content
        EXPORTING
          ex_t_output     TYPE gty_t_output,
      parse_purchase_order
        IMPORTING
          im_s_po_content TYPE gty_po_content
        EXPORTING
          ex_ret_code     TYPE i
          ex_err_text     TYPE string
          ex_t_po_output  TYPE /goog/cl_documentai_v1=>ty_085,
      parse_purchase_order_gemini
        IMPORTING
          im_s_po_content TYPE gty_po_content
        EXPORTING
          ex_ret_code     TYPE i
          ex_err_text     TYPE string
          ex_t_po_output  TYPE /goog/cl_documentai_v1=>ty_085,
      get_customer_from_address
        IMPORTING
          im_address           TYPE string
        EXPORTING
          ex_formatted_address TYPE string
          ex_customer_number   TYPE kunnr,
      translate_delivery_comments
        EXPORTING
          im_source_language TYPE string
        CHANGING
          ch_comments        TYPE string,
      detect_comments_language
        IMPORTING
          im_comments        TYPE string
        RETURNING
          VALUE(rv_language) TYPE string,
      create_sales_order
        IMPORTING
          im_documentdata TYPE gty_documentdata
        EXPORTING
          ex_error        TYPE string
          ex_vbeln        TYPE vbeln_va,
      convert_date
        IMPORTING
          iv_in_date         TYPE string
        RETURNING
          VALUE(rv_out_date) TYPE datum,
      get_order_header_text
        IMPORTING
          iv_textid        TYPE tdid
          iv_text          TYPE string
          iv_langu         TYPE string
        CHANGING
          VALUE(ex_t_text) TYPE bapisdtext_t,
      send_to_pubsub
        IMPORTING
          iv_sales_order_number    TYPE vbeln
          iv_purchase_order_number TYPE bstkd
          iv_delivery_block        TYPE bezei_lifsp
          iv_explanation           type string
          iv_recommendation        type string,

      display_output
        IMPORTING
          im_t_output TYPE gty_t_output,
      validate_auto_po_rules
        IMPORTING
          im_documentdata TYPE gty_documentdata
        EXPORTING
          ex_rule_failed  TYPE char1
          ex_explanation  TYPE string
          ex_recommendation type string.


ENDCLASS.
