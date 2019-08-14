*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDIMENSION01
*   generation date: 10.06.2009 at 18:40:11 by user D00211
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDIMENSION01       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
