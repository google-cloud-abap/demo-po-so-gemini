*&---------------------------------------------------------------------*
*& Include          ZGOOG_SAPPHIRE_UNBLOCK_SO_TOP
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
    p_sub_id TYPE string OBLIGATORY LOWER CASE.

SELECTION-SCREEN END OF BLOCK b1.

DATA:
        go_unblock_so TYPE REF TO lcl_main.
