*&---------------------------------------------------------------------*
*& Include          ZGOOG_SAPPHIRE_UNBLOCK_CD
*&---------------------------------------------------------------------*

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      execute,
      close_connection.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_so_unblock_request,
        sales_order    TYPE string,
        purchase_order TYPE string,

      END OF gty_so_unblock_request,

      BEGIN OF gty_output,
        sales_order    TYPE string,
        purchase_order TYPE string,
        status         TYPE string,

      END OF gty_output,

      gtt_so_unblock_request TYPE STANDARD TABLE OF gty_so_unblock_request,
      gtt_output             TYPE STANDARD TABLE OF gty_output.

    DATA:
          mo_pubsub_client    TYPE REF TO /goog/cl_pubsub_v1.

    METHODS:
      pull_so_unblock_request
        EXPORTING
          ex_error              TYPE string
          et_so_unblock_request TYPE gtt_so_unblock_request,

      unblock_sales_order
        IMPORTING
          it_so_unblock_request TYPE gtt_so_unblock_request
        EXPORTING
          et_output             TYPE gtt_output,

      display_output
        IMPORTING
          im_t_output TYPE gtt_output.

ENDCLASS.
