*&---------------------------------------------------------------------*
*& Report ZGOOG_PROCESS_PO_AUTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_sapphire_po_with_gemini.

INCLUDE ZGOOG_SAPPHIRE_PO_AUTO_G_DEF.
INCLUDE ZGOOG_SAPPHIRE_PO_AUTO_G_SEL.
INCLUDE ZGOOG_SAPPHIRE_PO_AUTO_G_IMPL.

START-OF-SELECTION.
  DATA(go_process_po_auto) = NEW lcl_main(  ).
  go_process_po_auto->execute( ).

END-OF-SELECTION.
  go_process_po_auto->close_connection( ).
