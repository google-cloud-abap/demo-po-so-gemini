*&---------------------------------------------------------------------*
*& Report ZGOOG_SAPPHIRE_UNBLOCK_SO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_sapphire_unblock_so.

INCLUDE zgoog_sapphire_unblock_cd.
INCLUDE zgoog_sapphire_unblock_so_top.
INCLUDE zgoog_sapphire_unblock_ci.

START-OF-SELECTION.

  CREATE OBJECT go_unblock_so.
  go_unblock_so->execute( ).

END-OF-SELECTION.
  go_unblock_so->close_connection( ).
