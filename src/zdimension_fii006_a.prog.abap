*&---------------------------------------------------------------------*
*& Report  ZDIMENSION_FII006_A
*&Project Name          :  达美银行余额调节表                            *
* Created by            :  D00194                           *
* Create on             :  2009-05-04
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDIMENSION_FII006_A.
*&---------------------------------------------------------------------*
*& 数据定义
*&---------------------------------------------------------------------*
TABLES t001.
TYPE-POOLS ole2 .

DATA: v_excel       TYPE ole2_object,    " Excel object
  l_window      TYPE ole2_object,    " list of workbooks
  l_books       TYPE ole2_object,    " list of workbooks
  l_book        TYPE ole2_object,    " workbook
  l_cell        TYPE ole2_object,    " cell object
  l_font        TYPE ole2_object,    " font object
  l_column      TYPE ole2_object,    " COLUMN OBJECT
  l_row         TYPE ole2_object,    " COLUMN OBJECT
  l_range       TYPE ole2_object,    " RANGE OBJECT
  l_format      TYPE ole2_object,    " RANGE OBJECT
  l_border      TYPE ole2_object,    " BORDER OBJECT
  l_edgebottom  TYPE ole2_object,    " BORDER OBJECT
  l_sheet       TYPE ole2_object,    " cell object
  l_pagesetup   TYPE ole2_object,    " font object
  l_interior    TYPE ole2_object.

DATA:
  BEGIN OF i_out OCCURS 0,
    sdate TYPE sy-datum,
    stext TYPE char40,
    sidid TYPE char12,
    sdmba TYPE dmbtr,
    sdmbm TYPE dmbtr,
    bdate TYPE sy-datum,
    btext TYPE char40,
    belnr TYPE char10,
    bdmba TYPE dmbtr,
    bdmbm TYPE dmbtr,
  END OF i_out.

DATA:
  v_lines    TYPE i,
  v_title(72) TYPE c,
  v_sap_hsl  TYPE bapi1028_3-balance,"企业余额
  v_bank_hsl TYPE bapi1028_3-balance,"银行余额
  v_sap_chg  TYPE bapi1028_3-balance,
  v_bank_chg TYPE bapi1028_3-balance,
  v_txt50 TYPE char50,
  v_uscod(30)  TYPE c.
* 设置电子表格单元格属性
DATA g_waers TYPE waers.

DATA p_rldnr TYPE FAGL_RLDNR.
DATA p_hkont TYPE hkont.

DEFINE m_set_property.
  set property of &1 &2 = &3 no flush .
  perform frm_err_hdl.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& 设置 SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME .
  PARAMETERS:
    p_bukrs TYPE bukrs OBLIGATORY,
    "p_hkont TYPE hkont OBLIGATORY ,"MATCHCODE OBJECT zhkont,
    p_uscod TYPE zdimuscod OBLIGATORY,""
    "p_rldnr TYPE FAGL_RLDNR OBLIGATORY,""
    p_gjahr TYPE gjahr OBLIGATORY,
    p_monat TYPE rpmax OBLIGATORY.

SELECTION-SCREEN END OF BLOCK blk1.


*&---------------------------------------------------------------------*
*& 执行程序事件
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*------------guwen482---add-AUTHORITY-CHECK---20090114-------------------------

  DATA: BEGIN OF i_bukrs OCCURS 0,
           bukrs LIKE t001k-bukrs,
           werks LIKE t001k-bwkey,"评估范围
        END OF i_bukrs.
  DATA:g_str TYPE string.

  SELECT bukrs
*         bwkey AS werks
  INTO TABLE i_bukrs
  FROM t001
  WHERE bukrs = p_bukrs
  .
  IF LINES( i_bukrs ) = 0.
    MESSAGE '请重新选择条件，不存在公司条件' TYPE 'I'.
    STOP.
  ELSE.
*    LOOP AT i_bukrs.
*      AUTHORITY-CHECK OBJECT 'Z_AUTH_CHK'
*               ID 'BUKRS' FIELD i_bukrs-bukrs
**               ID 'WERKS' FIELD i_bukrs-werks
**               ID 'VKORG' FIELD '*'
**               ID 'VTWEG' FIELD '*'
**               ID 'EKORG' FIELD '*'
*               .
*      g_str = i_bukrs-bukrs.
*      IF sy-subrc NE 0.
*        CONCATENATE  '没有' g_str '公司的权限' INTO g_str.
*        "MESSAGE g_str TYPE 'I'.
*        "STOP.
*      ENDIF.
*    ENDLOOP.
  ENDIF.
*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------------

at selection-screen.
  PERFORM pur_check."权限检查
  PERFORM f_get_hkont. "获得科目
  PERFORM f_get_ldgrp. "分类帐
  PERFORM f_get_data.   "数据取得
  PERFORM f_print.      "EXCEL输出
END-OF-SELECTION.
*-------------------------------------------------------------------------------
at SELECTION-SCREEN on VALUE-REQUEST FOR p_USCOD.
  perform FRM_GET_zdimuscod using p_USCOD.

*&---------------------------------------------------------------------*
*&      Form  frm_err_hdl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_err_hdl .
IF sy-subrc <> 0.
    WRITE: / 'Fehler bei OLE-Automation: '.
    CASE sy-subrc.
      WHEN 1.
        WRITE : '1: ', sy-msgli.
      WHEN 2.
        WRITE '2 : A method call resulted in an error'.
      WHEN 3.
        WRITE '3 : Setting a property resulted in an error.'.
      WHEN 4.
        WRITE '4 : Reading a property resulted in an error.'.
      WHEN OTHERS.
        WRITE sy-subrc.
    ENDCASE.
    STOP.
  ENDIF.

ENDFORM.                    " frm_err_hdl
*&---------------------------------------------------------------------*
*&      Form  pur_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pur_check .
data : msg type string.
  AUTHORITY-CHECK OBJECT 'Z_FI_BANK' ID 'BUKRS' FIELD p_bukrs
                                     ID 'SAKNR' field p_hkont.
  IF sy-subrc <> 0.
    concatenate '你没有公司代码' p_bukrs
                '及其对应科目' p_hkont '的权限！' into msg.
    "MESSAGE e888(sabapdocu) WITH msg.
    "stop.

  ENDIF.

ENDFORM.                    " pur_check
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .
**** 抬头取得
  PERFORM f_get_txt.

**** 余额取得
  PERFORM f_get_hsl.

**** 企业未达明细（银行账单中未对账的明细）
  PERFORM f_get_sap_left.

**** 银行未达明细（企业账单中未对账的明细）
  PERFORM f_get_bank_right.

**** 明细合计设定
  PERFORM f_set_total.

**** 调节后余额标志设定
  PERFORM f_set_buhk.

ENDFORM.                    " f_get_data
*&---------------------------------------------------------------------*
*&      Form  f_get_txt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_txt .
DATA:l_zdimbuhk TYPE zdimbuhk,
       l_monat(2) TYPE n.
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF l_zdimbuhk
    FROM zdimbuhk
   WHERE bukrs = p_bukrs
     AND hkont = p_hkont
     AND zdimuscod = p_uscod
    .
  IF sy-subrc = 0.
    v_uscod = p_uscod.""l_zdimbuhk-zdimuscod.
    v_txt50 = l_zdimbuhk-txt50.
  ELSE.
    MESSAGE i888(sabapdocu) WITH '科目与账号未维护！'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  l_monat = p_monat.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  CONCATENATE t001-butxt
              '银行余额调节表'
              '(' p_gjahr '年' l_monat '月)'
            INTO v_title.

ENDFORM.                    " f_get_txt
*&---------------------------------------------------------------------*
*&      Form  f_get_hsl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_hsl .
****  企业余额取得
  PERFORM f_get_sap_hsl.

**** 银行余额取得
  PERFORM f_get_bank_hsl.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_get_sap_hsl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_sap_hsl .
DATA:
    l_hkont           TYPE char10,
    l_account_balance TYPE bapi1028_3-balance,
    l_account         TYPE TABLE OF zbapi_acc,
    l_temp             TYPE zbapi_acc.
  CLEAR:
    l_account_balance,
    l_account,
    l_temp.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = p_hkont
       IMPORTING
            output = l_hkont.

****  企业期初余额
  l_temp-gl_account = l_hkont.
  APPEND l_temp TO l_account.

  CALL FUNCTION 'ZDIMENSION_FED_GET_SUB_BALAN_A'""
       EXPORTING
            rclnt           = sy-mandt
            bukrs           = p_bukrs
            ryear           = p_gjahr
            rpmax           = p_monat
            rldnr           = p_rldnr
            uscod           = p_uscod
       IMPORTING
            account_balance = l_account_balance
       TABLES
            account         = l_account.
  IF sy-subrc = 0.
    v_sap_hsl = l_account_balance.
  ENDIF.

ENDFORM.                    " f_get_sap_hsl
*&---------------------------------------------------------------------*
*&      Form  f_get_bank_hsl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_bank_hsl .
DATA: l_zdimglt0    TYPE zdimglt0,
        l_bank_hsl  TYPE bapi1028_3-balance,
        l_ys(2)     TYPE n,                     "需汇总的月份数
        l_field(16),                            "表字段名
        l_hkont    TYPE char10.

  FIELD-SYMBOLS <fs>.                      "用于动态运算的指针
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = p_hkont
       IMPORTING
            output = l_hkont.

  SELECT *
    FROM zdimglt0
    INTO l_zdimglt0
   WHERE bukrs = p_bukrs
     AND hkont = l_hkont
     AND gjahr = p_gjahr
     AND zdimuscod = p_uscod
    .
    CLEAR l_ys.
    IF l_zdimglt0-shkzg = 'S'."计算本月借方余额
      DO p_monat TIMES.
        CLEAR l_field.
        l_ys = l_ys + 1.
        CONCATENATE 'L_zdimGLT0-HSL' l_ys INTO l_field.
        ASSIGN (l_field) TO <fs>.
        l_bank_hsl = l_bank_hsl - <fs>.
      ENDDO.
    ENDIF.

    IF l_zdimglt0-shkzg = 'H'."计算本月贷方余额.
      DO p_monat TIMES.
        CLEAR l_field.
        l_ys = l_ys + 1.
        CONCATENATE 'L_ZDIMGLT0-HSL' l_ys INTO l_field.
        ASSIGN (l_field) TO <fs>.
        l_bank_hsl = l_bank_hsl + <fs>.
      ENDDO.
    ENDIF.
  ENDSELECT.

  v_bank_hsl = l_zdimglt0-hslvt + l_bank_hsl.

ENDFORM.                    " f_get_bank_hsl
*&---------------------------------------------------------------------*
*&      Form  f_get_sap_left
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_sap_left .
DATA:
    lt_zdimwsbk TYPE TABLE OF zdimwsbk,
    l_zdimwsbk TYPE zdimwsbk,
    l_dtext TYPE zdimwseg-zdimdtext.
****查找银行未清企业未达明细
  SELECT *
    FROM zdimwsbk
    INTO CORRESPONDING FIELDS OF TABLE lt_zdimwsbk
   WHERE bukrs = p_bukrs
     AND hkont = p_hkont
     AND zdimflag = space
     AND zdimuscod = p_uscod
     AND ( ( gjahr < p_gjahr ) OR ( gjahr = p_gjahr ) AND ( monat <= p_monat ) )
    .

    SORT lt_zdimwsbk BY zdimedate ASCENDING.

  LOOP AT lt_zdimwsbk INTO l_zdimwsbk.
    "获得货币种类 added by d00194 at 090611
    g_waers = l_zdimwsbk-waers.
****添加到输出表中
    i_out-sdate = l_zdimwsbk-zdimedate.
*    I_OUT-STEXT = L_zdimWSBK-DTEXT.
    i_out-stext = l_zdimwsbk-zdimname.
    i_out-sidid = l_zdimwsbk-zdimcid.

    IF l_zdimwsbk-shkzg = 'S'.
      i_out-sdmbm = l_zdimwsbk-dmbtr.
    ELSE.
      i_out-sdmba = l_zdimwsbk-dmbtr.
    ENDIF.

    APPEND i_out.
    CLEAR i_out.
  ENDLOOP.

ENDFORM.                    " f_get_sap_left
*&---------------------------------------------------------------------*
*&      Form  f_get_bank_right
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_bank_right .
DATA:
    lt_zdimbsbs TYPE TABLE OF zdimbsbs,
    l_zdimbsbs TYPE zdimbsbs,
    l_sgtxt TYPE bsis-sgtxt,
    l_bldat TYPE bsis-bldat,
    l_ind TYPE i VALUE '1',
    l_bsis TYPE bsis,
    lt_bsis TYPE TABLE OF bsis,
    l_bsas TYPE bsas,
    lt_bsas TYPE TABLE OF bsas.
****查找企业未清银行未达明细
  SELECT *
    FROM zdimbsbs
    INTO CORRESPONDING FIELDS OF TABLE lt_zdimbsbs
   WHERE bukrs = p_bukrs
     AND hkont = p_hkont
     AND zdimflag = space
     AND zdimuscod = p_uscod
     AND ( ( gjahr < p_gjahr ) OR ( gjahr = p_gjahr ) AND ( monat <= p_monat ) )
    .
 SORT lt_zdimbsbs BY BLDAT ASCENDING.

  LOOP AT lt_zdimbsbs INTO l_zdimbsbs.
****添加到输出表中
    READ TABLE i_out INDEX l_ind.
    IF sy-subrc = 0.
      i_out-bdate = l_zdimbsbs-bldat.
      i_out-btext = l_zdimbsbs-sgtxt.
      i_out-belnr = l_zdimbsbs-belnr.

      IF l_zdimbsbs-shkzg = 'S'.
        i_out-bdmba = l_zdimbsbs-dmbtr.
      ELSE.
        i_out-bdmbm = l_zdimbsbs-dmbtr.
      ENDIF.

      MODIFY i_out INDEX sy-tabix
      TRANSPORTING bdate btext belnr bdmba bdmbm.
    ELSE.
      i_out-bdate = l_zdimbsbs-bldat.
      i_out-btext = l_zdimbsbs-sgtxt.
      i_out-belnr = l_zdimbsbs-belnr.

      IF l_zdimbsbs-shkzg = 'S'.
        i_out-bdmba = l_zdimbsbs-dmbtr.
      ELSE.
        i_out-bdmbm = l_zdimbsbs-dmbtr.
      ENDIF.
      APPEND i_out.
    ENDIF.

    l_ind = l_ind + 1.
    CLEAR i_out.
  ENDLOOP.

ENDFORM.                    " f_get_bank_right
*&---------------------------------------------------------------------*
*&      Form  f_set_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_total .
DATA:
    l_sdmba TYPE bapi1028_3-balance,
    l_sdmbm TYPE bapi1028_3-balance,
    l_bdmba TYPE bapi1028_3-balance,
    l_bdmbm TYPE bapi1028_3-balance.

  CLEAR i_out.

  LOOP AT i_out.
****清空初始日期
    IF i_out-sdate IS INITIAL.
      i_out-sdate = space.
      MODIFY i_out TRANSPORTING sdate.
    ENDIF.

    IF i_out-bdate IS INITIAL.
      i_out-bdate = space.
      MODIFY i_out TRANSPORTING bdate.
    ENDIF.

    AT LAST.
      SUM.
      l_sdmba = i_out-sdmba.
      l_sdmbm = i_out-sdmbm.
      l_bdmba = i_out-bdmba.
      l_bdmbm = i_out-bdmbm.
    ENDAT.
  ENDLOOP.


  i_out-sdate = space.
  i_out-stext = '合计'.
  i_out-sidid = space.
  i_out-sdmba = l_sdmba.
  i_out-sdmbm = l_sdmbm.
  i_out-bdate = space.
  i_out-btext = space.
  i_out-belnr = space.
  i_out-bdmba = l_bdmba.
  i_out-bdmbm = l_bdmbm.

  APPEND i_out.
**** 调整后余额设定
  v_sap_chg = v_sap_hsl + l_sdmba - l_sdmbm.
  v_bank_chg = v_bank_hsl + l_bdmba - l_bdmbm.

ENDFORM.                    " f_set_total
*&---------------------------------------------------------------------*
*&      Form  f_set_buhk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_buhk .
DATA: l_zdimbuhk TYPE zdimbuhk.

  SELECT SINGLE * FROM zdimbuhk
      INTO l_zdimbuhk
     WHERE bukrs = p_bukrs
       AND hkont = p_hkont.

  IF v_sap_chg <> v_bank_chg.
    l_zdimbuhk-zdimflag = 'X'.                "调节后余额不平
    UPDATE zdimbuhk FROM l_zdimbuhk.
  ELSE.
    l_zdimbuhk-zdimflag = ' '.                "调节后余额平
    UPDATE zdimbuhk FROM l_zdimbuhk.
  ENDIF.

ENDFORM.                    " f_set_buhk
*&---------------------------------------------------------------------*
*&      Form  f_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_print .
DATA: l_pos TYPE string,
        l_line_c TYPE string.
  CREATE OBJECT v_excel 'EXCEL.APPLICATION'.
  PERFORM frm_err_hdl.

  CALL METHOD OF v_excel 'Workbooks' = l_books NO FLUSH.
  PERFORM frm_err_hdl.

* 创建一个sheet
  CALL METHOD OF l_books 'Add' = l_book NO FLUSH.
  PERFORM frm_err_hdl.

  CALL METHOD OF v_excel 'ActiveWindow' = l_window NO FLUSH.
  PERFORM frm_err_hdl.
  m_set_property l_window 'DisplayZeros'  0.

  m_set_property v_excel 'Visible' 0.
*/-------------------------------------------------
*  设置所有单元格的属性
  CALL METHOD OF v_excel 'Cells' = l_cell NO FLUSH.
  PERFORM frm_err_hdl.
  m_set_property l_cell 'HorizontalAlignment' 3.
  m_set_property l_cell 'VerticalAlignment'   2.
  GET PROPERTY OF l_cell 'Font' = l_font no flush.
  PERFORM frm_err_hdl.
  m_set_property l_font 'Name'  '宋体'.
  m_set_property l_font 'Size'  9 .
  m_set_property l_cell 'WrapText' 1.

*设置每列属性
  PERFORM f_getcolum .

*对大标题所在行进行属性设置
  CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
    EXPORTING #1 = 'A1' #2 = 'I1'.
  PERFORM frm_err_hdl.
  m_set_property l_range 'MergeCells' 1 .
  m_set_property l_range 'HorizontalAlignment'  3.
  m_set_property l_range 'VerticalAlignment'    2.

  GET PROPERTY OF l_range 'Font' = l_font no flush .
  PERFORM frm_err_hdl.
  m_set_property l_font 'Bold'  2 .
  m_set_property l_font 'Size'  20.
  m_set_property l_range 'Value' v_title.

****余额货币设置
  CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
    EXPORTING #1 = 'B4' #2 = 'B6'.
  PERFORM frm_err_hdl.
  SET PROPERTY OF l_range 'NumberFormat' = '#,##0.00'.

  CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
    EXPORTING #1 = 'F4' #2 = 'G6'.
  PERFORM frm_err_hdl.
  SET PROPERTY OF l_range 'NumberFormat' = '#,##0.00'.

*红字进行设置
  CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
    EXPORTING #1 = 'D7' #2 = 'D9'.
  PERFORM frm_err_hdl.
  GET PROPERTY OF l_range 'Font' = l_font no flush .
  PERFORM frm_err_hdl.
  m_set_property l_font 'ColorIndex'  3 .

  CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
    EXPORTING #1 = 'I7' #2 = 'I9'.
  PERFORM frm_err_hdl.
  GET PROPERTY OF l_range 'Font' = l_font no flush .
  PERFORM frm_err_hdl.
  m_set_property l_font 'ColorIndex'  3 .
*标题行第二，三行进行设置行高
  PERFORM row_high USING 2 '15'.
  PERFORM frm_err_hdl.

  PERFORM row_high USING 3 '15'.
  PERFORM frm_err_hdl.

  PERFORM f_gettitle.
*------------------------处理数据部分----------------------------------*
  v_lines = 10.
  PERFORM f_result USING v_lines.
  v_lines = v_lines - 1.
  PERFORM border USING 2 v_lines .

  v_lines = v_lines + 3.

  l_line_c = v_lines.
  CONCATENATE 'F' l_line_c INTO l_pos.
  PERFORM merge_cell  USING l_pos l_pos '审核人：'.

  l_line_c = l_line_c + 2.
  CONCATENATE 'F' l_line_c INTO l_pos.
  PERFORM merge_cell  USING l_pos l_pos '审核日期：'.

  l_line_c = l_line_c + 2.
  CONCATENATE 'F' l_line_c INTO l_pos.
  PERFORM merge_cell  USING l_pos l_pos '制单人：'.

  l_line_c = l_line_c + 2.
  CONCATENATE 'F' l_line_c INTO l_pos.
  PERFORM merge_cell  USING l_pos l_pos '制单日期：'.

  SET PROPERTY OF v_excel  'Visible' = 1 .
  FREE OBJECT v_excel.

ENDFORM.                    " f_print
*&---------------------------------------------------------------------*
*&      Form  border
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2      text
*      -->P_V_LINES  text
*----------------------------------------------------------------------*
FORM border  USING    x y.
DATA: l_c1 TYPE string,
        l_c2 TYPE string.

  l_c1 = x.
  l_c2 = y.

  CONCATENATE 'A' l_c1 INTO l_c1.
  CONCATENATE 'I' l_c2 INTO l_c2.

  CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
       EXPORTING #1 = l_c1 #2 = l_c2.
  PERFORM frm_err_hdl.
* 设置边界线
**设置外框左边(xlEdgeLeft)线条的线型和粗细
  CALL METHOD OF l_range 'BORDERS' = l_border EXPORTING #1 = '7'.
  SET PROPERTY OF l_border 'LineStyle' = '1'.
  SET PROPERTY OF l_border 'WEIGHT' = 2.                    "4=max

**设置外框上边(xlEdgeTop)线条的线型和粗细
  CALL METHOD OF l_range 'BORDERS' = l_border EXPORTING #1 = '8'.
  SET PROPERTY OF l_border 'LineStyle' = '1'.
  SET PROPERTY OF l_border 'WEIGHT' = 2.                    "4=max

**设置外框下边(xlEdgeBottom)线条的线型和粗细
  CALL METHOD OF l_range 'BORDERS' = l_border EXPORTING #1 = '9'.
  SET PROPERTY OF l_border 'LineStyle' = '1'.
  SET PROPERTY OF l_border 'WEIGHT' = 2.                    "4=max

**设置外框右边(xlEdgeRight)线条的线型和粗细
  CALL METHOD OF l_range 'BORDERS' = l_border EXPORTING #1 = '10'.
  SET PROPERTY OF l_border 'LineStyle' = '1'.
  SET PROPERTY OF l_border 'WEIGHT' = 2.                    "4=max

**设置内框垂直(xlInsideVertical)线条的线型和粗细
  CALL METHOD OF l_range 'BORDERS' = l_border EXPORTING #1 = '11'.
  SET PROPERTY OF l_border 'LineStyle' = '1'.
  SET PROPERTY OF l_border 'WEIGHT' = 2.                    "4=max

**设置内框水平(xlInsideHorizontal)线条的线型和粗细
  CALL METHOD OF l_range 'BORDERS' = l_border EXPORTING #1 = '12'.
  SET PROPERTY OF l_border 'LineStyle' = '1'.
  SET PROPERTY OF l_border 'WEIGHT' = 2.                    "4=max


ENDFORM.                    " border
*&---------------------------------------------------------------------*
*&      Form  f_getcolum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_getcolum .
*根据字段位置给每行每列赋值
  DATA:tem_step TYPE i,
       tem_col(2).
  tem_step = 1.
  DO 9 TIMES.
    CASE tem_step.
      WHEN 1 OR 5.
        tem_col = '10'.
        CALL METHOD OF v_excel 'Columns' = l_column NO FLUSH
                                EXPORTING #1 = tem_step.
        PERFORM frm_err_hdl.
        SET PROPERTY OF l_column 'ColumnWidth' = tem_col  no flush.
        PERFORM frm_err_hdl.
        SET PROPERTY OF l_column 'NumberFormat' = 'yyyy-m-d'.
        PERFORM frm_err_hdl.
      WHEN 2 OR 6.
        tem_col = '20'.
        CALL METHOD OF v_excel 'Columns' = l_column NO FLUSH
                                EXPORTING #1 = tem_step.
        PERFORM frm_err_hdl.
        SET PROPERTY OF l_column 'ColumnWidth' = tem_col  no flush.
        PERFORM frm_err_hdl.
      WHEN 7.
        tem_col = '20'.
        CALL METHOD OF v_excel 'Columns' = l_column NO FLUSH
                                EXPORTING #1 = tem_step.
        PERFORM frm_err_hdl.
        SET PROPERTY OF l_column 'ColumnWidth' = tem_col  no flush.
        PERFORM frm_err_hdl.
        SET PROPERTY OF l_column 'NumberFormat' = '@'.
        PERFORM frm_err_hdl.
      WHEN 3 OR 4 OR 8 OR 9.
        tem_col = '10'.
        CALL METHOD OF v_excel 'Columns' = l_column NO FLUSH
                                EXPORTING #1 = tem_step.
        PERFORM frm_err_hdl.
        SET PROPERTY OF l_column 'ColumnWidth' = tem_col  no flush.
        PERFORM frm_err_hdl.
        SET PROPERTY OF l_column 'NumberFormat' = '#,##0.00'.
        PERFORM frm_err_hdl.
    ENDCASE.
    tem_step = tem_step + 1.
  ENDDO.

ENDFORM.                    " f_getcolum
*&---------------------------------------------------------------------*
*&      Form  f_gettitle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gettitle .
DATA:l_date(20) TYPE c.
  DATA: l_text(70) TYPE c,
        l_hkont(70) TYPE c,
        l_uscod(72) TYPE c,
        l_hkont_temp TYPE char10,
        l_waers TYPE char5.
* ALPHA 补零
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = p_hkont
       IMPORTING
            output = l_hkont_temp.

* 合成输出文本
  CONCATENATE: '公司代码' p_bukrs INTO l_text,
               '会计科目及描述' l_hkont_temp v_txt50 INTO l_hkont,
               '银行账号' v_uscod INTO l_uscod,
               '货币' g_waers INTO l_waers
               .


*标题第二行(左对齐)
  PERFORM merge_cell2 USING 'A2' 'D2' l_text.
  PERFORM merge_cell2 USING 'E2' 'G3' l_uscod.
  PERFORM merge_cell2 USING 'H2' 'I3' l_waers.
*表头行
  PERFORM merge_cell2  USING 'A3' 'D3' l_hkont.
  PERFORM merge_cell  USING 'A4' 'A6' '企业银行余额'.
  PERFORM merge_cell  USING 'B4' 'B6' v_sap_hsl.
  PERFORM merge_cell  USING 'C4' 'C6' '调整后余额'.
  PERFORM merge_cell  USING 'D4' 'D6' v_sap_chg.
  PERFORM merge_cell  USING 'E4' 'E6' '银行账单余额'.
  PERFORM merge_cell  USING 'F4' 'G6' v_bank_hsl.
  PERFORM merge_cell  USING 'H4' 'H6' '调整后余额'.
  PERFORM merge_cell  USING 'I4' 'I6' v_bank_chg.

  PERFORM merge_cell  USING 'A7' 'A9' '日期'.
  PERFORM merge_cell  USING 'B7' 'B9' '文本摘要'.
  PERFORM merge_cell  USING 'C7' 'C9' '加：单位未收银行已收'.
  PERFORM merge_cell  USING 'D7' 'D9' '减：单位未付银行已付'.
  PERFORM merge_cell  USING 'E7' 'E9' '日期'.
  PERFORM merge_cell  USING 'F7' 'F9' '文本摘要'.
  PERFORM merge_cell  USING 'G7' 'G9' '凭证号'.
  PERFORM merge_cell  USING 'H7' 'H9' '加：银行未收单位已收'.
  PERFORM merge_cell  USING 'I7' 'I9' '减；银行未付单位已付'.

ENDFORM.                    " f_gettitle
*&---------------------------------------------------------------------*
*&      Form  f_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_LINES  text
*----------------------------------------------------------------------*
FORM f_result  USING    P_V_LINES.
LOOP AT i_out.
    PERFORM row_high USING p_v_lines '15'.
    PERFORM down USING p_v_lines 1  i_out-sdate     2.
    PERFORM down USING p_v_lines 2  i_out-stext     2.
    PERFORM down USING p_v_lines 3  i_out-sdmba  4.
    PERFORM down USING p_v_lines 4  i_out-sdmbm  4.
    PERFORM down USING p_v_lines 5  i_out-bdate  2.
    PERFORM down USING p_v_lines 6  i_out-btext  2.
    PERFORM down USING p_v_lines 7  i_out-belnr  2.
    PERFORM down USING p_v_lines 8  i_out-bdmba  4.
    PERFORM down USING p_v_lines 9  i_out-bdmbm 4.
*   设置行高
    PERFORM row_high USING p_v_lines '15'.
    p_v_lines = p_v_lines + 1.
    CLEAR i_out.
  ENDLOOP.

ENDFORM.                    " f_result
*&---------------------------------------------------------------------*
*&      Form  down
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_V_LINES  text
*      -->P_1      text
*      -->P_I_OUT_SDATE  text
*      -->P_2      text
*----------------------------------------------------------------------*
FORM down  USING    x y i_value i_align.
CALL METHOD OF v_excel 'CELLS' = l_cell NO FLUSH
        EXPORTING #1 = x #2 = y.
  SET PROPERTY OF l_cell 'HorizontalAlignment' = i_align no flush .
  PERFORM frm_err_hdl.
  SET PROPERTY OF l_cell 'VALUE' = i_value no flush.

ENDFORM.                    " down
*&---------------------------------------------------------------------*
*&      Form  merge_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_POS  text
*      -->P_L_POS  text
*      -->P_1352   text
*----------------------------------------------------------------------*
FORM merge_cell  USING    x y i_value.
CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
      EXPORTING #1 = x #2 = y.
  PERFORM frm_err_hdl.
  SET PROPERTY OF l_range 'MergeCells' = 1 no flush .
  SET PROPERTY OF l_range 'HorizontalAlignment' = 3 no flush .
  SET PROPERTY OF l_range 'VerticalAlignment' = 2 no flush .
  SET PROPERTY OF l_range 'Value' = i_value no flush .

ENDFORM.                    " merge_cell
*&---------------------------------------------------------------------*
*&      Form  merge_cell2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1847   text
*      -->P_1848   text
*      -->P_L_TEXT  text
*----------------------------------------------------------------------*
FORM merge_cell2  USING    x y i_value.
CALL METHOD OF v_excel 'Range' = l_range NO FLUSH
      EXPORTING #1 = x #2 = y.
  SET PROPERTY OF l_range 'MergeCells' = 1 no flush .
  SET PROPERTY OF l_range 'HorizontalAlignment' = 2 no flush .
  SET PROPERTY OF l_range 'VerticalAlignment' = 2 no flush .
  SET PROPERTY OF l_range 'Value' = i_value no flush .
  GET PROPERTY OF l_range 'Font' = l_font no flush .
  PERFORM frm_err_hdl.
  m_set_property l_font 'Bold'  2 .
  m_set_property l_font 'Size'  11.

ENDFORM.                    " merge_cell2
*&---------------------------------------------------------------------*
*&      Form  row_high
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2      text
*      -->P_1305   text
*----------------------------------------------------------------------*
FORM row_high  USING    x i_high.
CALL METHOD OF v_excel 'Rows' = l_row NO FLUSH
                                    EXPORTING #1 = x.
  PERFORM frm_err_hdl.
  SET PROPERTY OF l_row 'RowHeight' = i_high  no flush.
  PERFORM frm_err_hdl.

ENDFORM.                    " row_high
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_zdimuscod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_USCOD  text
*----------------------------------------------------------------------*
FORM FRM_GET_zdimuscod  USING    P_ZUSCOD.
  data: begin of it_zuscod occurs 0,
        BUKRS like ZDIMBUHK-bukrs,
        HKONT like ZDiMBUHK-hkont,
        TXT50 like ZDiMBUHK-txt50,
        ZDIMUSCOD like ZDiMBUHK-zdimuscod,
        uscodTXT  like ZDiMBUHK-txt50,
      end of it_zuscod.

data: i_bukrs like bseg-bukrs.
  DATA: BEGIN OF DYNPFIELDS OCCURS 5.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF DYNPFIELDS.
  data: RETURN_TAB LIKE  table of DDSHRETVAL with header line.
  FIELD-SYMBOLS: <fs_zuscod> like it_zuscod.
  clear: it_zuscod[],
         it_zuscod.
  MOVE 'P_BUKRS' TO DYNPFIELDS-FIELDNAME.
  APPEND DYNPFIELDS.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME               = SY-CPROG
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 01
      INVALID_DYNPROFIELD  = 02
      INVALID_DYNPRONAME   = 03
      INVALID_DYNPRONUMMER = 04
      INVALID_REQUEST      = 05
      NO_FIELDDESCRIPTION  = 06
      UNDEFIND_ERROR       = 07.

  IF SY-SUBRC EQ 0. "nur falls Transport-Felder im Dynpro existieren
    READ TABLE DYNPFIELDS WITH KEY 'P_BUKRS'.
    IF SY-SUBRC = 0.
      i_bukrs = DYNPFIELDS-FIELDVALUE.
    endif.
  endif.
  select * into CORRESPONDING FIELDS OF TABLE it_zuscod
    from ZDiMBUHK
    where BUKRS = i_bukrs
    AND zdimflag2 = 'X'
    .
  select single *
    from t001
    where BUKRS = i_bukrs.
  loop at it_zuscod ASSIGNING <fs_zuscod>.
    select single TXT50 into <fs_zuscod>-TXT50
      from SKAt
      where SPRAS = sy-langu
        and KTOPL = t001-KTOPL
        and SAKNR = <fs_zuscod>-hkont.
    select single TXT50 into <fs_zuscod>-uscodTXT
      from zdimbno
      where zdimuscod = <fs_zuscod>-zdimuscod.
  endloop.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZDIMUSCOD'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_ZUSCOD'  "??????
      value_org       = 'S'
    TABLES
      value_tab       = it_zuscod
      RETURN_TAB      = RETURN_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  else.
*    read table RETURN_TAB index 1.
*    P_ZUSCOD = RETURN_TAB-fieldval.
  endif.
ENDFORM.                    " FRM_GET_zdimuscod
*&---------------------------------------------------------------------*
*&      Form  f_get_ldgrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_ldgrp .
IF p_bukrs IS NOT INITIAL.
    SELECT SINGLE RLDNR INTO p_rldnr
      FROM zdimldgrp
      WHERE BUKRS = p_bukrs.
    IF sy-subrc <> 0.
      MESSAGE i888(sabapdocu) WITH  '未获得分类帐'.
      STOP.
    ENDIF.
  "ELSE."IF bukrs IS NOT INITIAL

  ENDIF."IF bukrs IS NOT INITIAL
ENDFORM.                    " f_get_ldgrp
*&---------------------------------------------------------------------*
*&      Form  f_get_hkont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_hkont .
  CLEAR p_hkont.
  SELECT SINGLE hkont FROM zdimbuhk
    INTO p_hkont
    WHERE bukrs = p_bukrs
    AND zdimuscod = p_uscod
    AND zdimflag2 = 'X'.
    IF sy-subrc ne 0.
      MESSAGE i888(sabapdocu) WITH text-111.
      stop.
    ENDIF.
ENDFORM.                    " f_get_hkont
