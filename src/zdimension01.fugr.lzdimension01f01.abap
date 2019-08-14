*----------------------------------------------------------------------*
***INCLUDE LZDIMENSION01F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  createnum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM createnum .
data: i_zdimcid  type zdimcid .
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZDIMFIRANG'
       IMPORTING
            number                  = i_zdimcid
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 zdimwkpf-zdimcid = i_zdimcid.
  LOOP AT it_zdimwseg.
    it_zdimwseg-zdimcid = i_zdimcid.
    it_zdimwseg-ZDIMLINE = sy-tabix.
    MODIFY it_zdimwseg.
  ENDLOOP.
ENDFORM.                    " createnum
*&---------------------------------------------------------------------*
*&      Form  frm_save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_save_data  CHANGING message type char50
                             return type c.
 REFRESH : itab1,it_zdimwsbk,it_zdimglt0.
 FREE itab1.
*  REFRESH i_zpglt0.
  PERFORM frm_f1_query_zdwmglt0.
  LOOP AT it_zdimwseg.
    CLEAR itab1.
    MOVE-CORRESPONDING it_zdimwseg TO itab1.
    itab1-ZDIMCID = it_zdimwseg-ZDIMCID.
    itab1-ZDIMLINE = it_zdimwseg-ZDIMLINE.
    APPEND itab1.
    MOVE-CORRESPONDING itab1 TO it_zdimwsbk.
    it_zdimwsbk-hkont = g_hkont.
    APPEND it_zdimwsbk.
*    itab-waste = l_waste.
*    itab-intnum = sy-tabix.
    CLEAR it_zdimglt0.
    it_zdimglt0-mandt = sy-mandt.
    it_zdimglt0-bukrs = it_zdimwseg-bukrs.
    it_zdimglt0-hkont = g_hkont.
    it_zdimglt0-txt50 = g_txt50.
    it_zdimglt0-zdimuscod = it_zdimwseg-zdimuscod.
    it_zdimglt0-gjahr = g_gjahr.
    it_zdimglt0-shkzg = it_zdimwseg-shkzg.
*    CLEAR zdimwkpf.
*    zdimwkpf-zdimuscod = l_bankn.
    zdimwkpf-mandt = sy-mandt.
*    zdimwkpf-bukrs = p_bukrs.
    zdimwkpf-ZDIMCID = it_zdimwseg-ZDIMCID.
    zdimwkpf-ZDIMEDATE = sy-datum.
    zdimwkpf-erdat = sy-datum.
    zdimwkpf-ernam = sy-uname.
    CASE g_monat.
      WHEN '01'.
        it_zdimglt0-hsl01 = it_zdimwseg-dmbtr.
      WHEN '02'.
        it_zdimglt0-hsl02 = it_zdimwseg-dmbtr.
      WHEN '03'.
        it_zdimglt0-hsl03 = it_zdimwseg-dmbtr.
      WHEN '04'.
        it_zdimglt0-hsl04 = it_zdimwseg-dmbtr.
      WHEN '05'.
        it_zdimglt0-hsl05 = it_zdimwseg-dmbtr.
      WHEN '06'.
        it_zdimglt0-hsl06 = it_zdimwseg-dmbtr.
      WHEN '07'.
        it_zdimglt0-hsl07 = it_zdimwseg-dmbtr.
      WHEN '08'.
        it_zdimglt0-hsl08 = it_zdimwseg-dmbtr.
      WHEN '09'.
        it_zdimglt0-hsl09 = it_zdimwseg-dmbtr.
      WHEN '10'.
        it_zdimglt0-hsl10 = it_zdimwseg-dmbtr.
      WHEN '11'.
        it_zdimglt0-hsl11 = it_zdimwseg-dmbtr.
      WHEN '12'.
        it_zdimglt0-hsl12 = it_zdimwseg-dmbtr.
      WHEN '13'.
        it_zdimglt0-hsl13 = it_zdimwseg-dmbtr.
      WHEN '14'.
        it_zdimglt0-hsl14 = it_zdimwseg-dmbtr.
      WHEN '15'.
        it_zdimglt0-hsl15 = it_zdimwseg-dmbtr.
      WHEN '16'.
        it_zdimglt0-hsl16 = it_zdimwseg-dmbtr.
    ENDCASE.
      COLLECT it_zdimglt0.
  ENDLOOP.
    MODIFY zdimwseg FROM TABLE it_zdimwseg.
    IF sy-subrc = 0.
      MODIFY zdimwsbk FROM TABLE it_zdimwsbk.
      IF sy-subrc = 0.
          MODIFY zdimglt0 FROM TABLE it_zdimglt0.
          IF sy-subrc = 0.
            MODIFY zdimwkpf.
            MESSAGE = text-106.
            return = 'S'.
            COMMIT WORK AND WAIT.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  REFRESH : itab1,it_zdimwsbk,it_zdimglt0.
  CLEAR : zdimwkpf.
ENDFORM.                    " frm_save_data
*&---------------------------------------------------------------------*
*&      Form  frm_f1_query_zdwmglt0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f1_query_zdwmglt0 .
 DATA l_gjahr LIKE bsis-gjahr.
  DATA l_hslvh LIKE glt0-hslvt.
  DATA l_hslvs LIKE glt0-hslvt.
  DATA l_monat LIKE bsis-monat.

  REFRESH it_zdimglt0.
  l_monat = g_monat.
  SELECT SINGLE  * FROM zdimglt0
      WHERE bukrs = zdimwkpf-bukrs AND
            hkont = g_hkont AND
            gjahr = g_gjahr.
  IF sy-subrc = 0.  "如果一月份不是第一次对帐，就不用再取上一年的记录
    CLEAR l_monat.
  ENDIF.
  IF l_monat = '01'.
    l_gjahr = g_gjahr - 1.
  ELSE.
    l_gjahr = g_gjahr.
  ENDIF.
  SELECT * FROM zdimglt0 " INTO TABLE i_zpglt
      WHERE bukrs = zdimwkpf-bukrs AND
            hkont = g_hkont AND
            gjahr = l_gjahr.

    IF l_monat = '01' AND zdimglt0-shkzg = 'S'.
      ADD zdimglt0-hslvt THEN zdimglt0-hsl01 UNTIL zdimglt0-hsl16 GIVING l_hslvs.
      MOVE-CORRESPONDING zdimglt0 TO it_zdimglt0.
    ELSEIF l_monat = '01' AND zdimglt0-shkzg = 'H'.
      ADD zdimglt0-hslvt THEN zdimglt0-hsl01 UNTIL zdimglt0-hsl16 GIVING l_hslvh.
      MOVE-CORRESPONDING zdimglt0 TO it_zdimglt0.
    ELSE.
      APPEND zdimglt0 TO it_zdimglt0.
    ENDIF.
  ENDSELECT.

  IF l_monat = '01'.
    IF l_hslvs GE l_hslvh.
      it_zdimglt0-shkzg = 'S'.
      it_zdimglt0-hslvt = l_hslvs - l_hslvh.
    ELSE.
      it_zdimglt0-shkzg = 'H'.
      it_zdimglt0-hslvt = l_hslvh - l_hslvs.
    ENDIF.
    CLEAR:it_zdimglt0-hsl01, it_zdimglt0-hsl02,it_zdimglt0-hsl03,it_zdimglt0-hsl04,
          it_zdimglt0-hsl05, it_zdimglt0-hsl06,it_zdimglt0-hsl07,it_zdimglt0-hsl08,
          it_zdimglt0-hsl09, it_zdimglt0-hsl10,it_zdimglt0-hsl11,it_zdimglt0-hsl12,
          it_zdimglt0-hsl13, it_zdimglt0-hsl14,it_zdimglt0-hsl15,it_zdimglt0-hsl16.
    APPEND it_zdimglt0.
  ELSE.

  ENDIF.
ENDFORM.                    " frm_f1_query_zdwmglt0
**&---------------------------------------------------------------------*
**&      Form  createnum
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM createnum .
*
*ENDFORM.                    " createnum
