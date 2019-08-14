*----------------------------------------------------------------------*
* Program Name          :  银行对账处理程序                            *
* Purpose               :                                              *
* Project Name          :  银行对账项目                                *
* Created by            :  卓成海                                      *
* Create on             :  2009-04-24                                  *
* Functional Consultant :                                              *
* Description           :                                              *
*----------------------------------------------------------------------*
*    Modification Log                                                  *
*Date        Programmer     Corr. #      Description                   *
*                                                                      *
*----------------------------------------------------------------------*

REPORT  ZDIMENSION_FII002_A.
*&---------------------------------------------------------------------*
*& 声明系统字典对象                                                    *
*&---------------------------------------------------------------------*
TABLES:zdimwkpf,
       zdimwseg,
       tcurc,
       alsmex_tabline,
       bkpf,
       t001,
       skb1,
       zdimldgrp,
       zdimscnro,
       zdimbuhk,
       zdimbsbs,
       zdimwsbk,
       zdimblock.
INCLUDE <icon>.
*&---------------------------------------------------------------------*
*& 本地数据结构                                                        *
*&---------------------------------------------------------------------*
TYPES:BEGIN OF typ_bsis,
        mandt LIKE bsis-mandt,
        bukrs LIKE bsis-bukrs,  "公司代码
        hkont LIKE bsis-hkont,  "会计科目
        zdimuscod type ZDIMUSCOD,
        gjahr LIKE bsis-gjahr,  "会计年度
        belnr LIKE bsis-belnr,  "会计凭证
        buzei LIKE bsis-buzei,  "凭证行项目
        budat LIKE bsis-budat,  "记帐日期
        bldat LIKE bsis-bldat,  "凭证日期
        monat LIKE bsis-monat,  "会计期间
        shkzg LIKE bsis-shkzg,  "借贷标识
        dmbtr LIKE bsis-dmbtr,  "凭证金额
        waers LIKE bsis-waers,  "货币类型
        xref3 LIKE bsis-xref3,  "参考码
        sgtxt LIKE bsis-sgtxt,
      END OF typ_bsis.
TYPES:BEGIN OF typ_bsis_status,
        bukrs LIKE bsis-bukrs,
        hkont LIKE bsis-hkont,
        gjahr LIKE bsis-gjahr,
        monat LIKE bsis-monat,
        belnr LIKE bsis-belnr,
        buzei LIKE bsis-buzei,
      END OF typ_bsis_status.
TYPES:BEGIN OF typ_bank,
        bukrs LIKE zdimwsbk-bukrs,   "公司代码
        ZDIMUSCOD LIKE zdimwsbk-ZDIMUSCOD,   "银行帐号
        gjahr LIKE zdimwsbk-gjahr,   "会计年度
        monat LIKE zdimwsbk-monat,   "会计期间
        ZDIMCID LIKE zdimwsbk-ZDIMCID,   "银行流水号
        ZDIMline LIKE zdimwsbk-ZDIMline, "银行流水号行项目
        ZDIMNAME LIKE zdimwsbk-ZDIMNAME,   "对方用户名
        zdimcscod LIKE zdimwsbk-zdimcscod,   "对方银行帐号
        ZDIMEDATE LIKE zdimwsbk-ZDIMEDATE,   "交易日期
        shkzg LIKE zdimwsbk-shkzg,   "借贷标识
        dmbtr LIKE zdimwsbk-dmbtr,   "金额
        ZDIMDTEXT LIKE zdimwsbk-ZDIMDTEXT,   "摘要
      END OF typ_bank.
TYPES:BEGIN OF typ_out,
       edit TYPE c,
       gjahr LIKE bsis-gjahr,
       monat LIKE bsis-gjahr,
       bukrs LIKE bsis-bukrs,  "公司代码
       budat LIKE bsis-budat,  "
       bldat LIKE bsis-bldat,  "
       belnr LIKE bsis-belnr,
       buzei LIKE bsis-buzei,
       hkont LIKE bsis-hkont,
       zdimuscod LIKE zdimwsbk-zdimuscod,  "银行帐号
       sgtxt LIKE bsis-sgtxt,
       waers LIKE bsis-waers,
       shkzg LIKE bsis-shkzg,
       dmbtr LIKE bsis-dmbtr,
       xref3 LIKE bsis-xref3,  "参考码
       ZDIMCID LIKE zdimwsbk-ZDIMCID,
       zdimline LIKE zdimwsbk-zdimline,
       gjahr1 LIKE zdimwsbk-gjahr,
       monat1 LIKE zdimwsbk-monat,
       ZDIMNAME LIKE zdimwsbk-ZDIMNAME,
       zdimcscod LIKE zdimwsbk-zdimcscod,
       zdimedate LIKE zdimwsbk-zdimedate,
       shkzg1 LIKE zdimwsbk-shkzg,
       dmbtr1 LIKE zdimwsbk-dmbtr,
       ZDIMDTEXT LIKE zdimwsbk-ZDIMDTEXT,
       ZDIMTYPE LIKE zdimbsbs-ZDIMTYPE,
       ZDIMFLAG LIKE zdimwsbk-ZDIMFLAG,
     END OF typ_out.
*&---------------------------------------------------------------------*
*& 变量与内表定义                                                      *
*&---------------------------------------------------------------------*
DATA it_bsis TYPE typ_bsis OCCURS 0 WITH HEADER LINE.        "会计凭证
DATA it_bsas TYPE typ_bsis_status OCCURS 0 WITH HEADER LINE. "会计凭证
DATA it_bank TYPE typ_bank OCCURS 0 WITH HEADER LINE.        "银行对帐单
DATA it_out TYPE typ_out OCCURS 0 WITH HEADER LINE.          "结果内表
DATA it_screen_out TYPE TABLE OF typ_out.   "屏幕内表
DATA it_out2 TYPE TABLE OF typ_out WITH HEADER LINE.
DATA wa_out TYPE typ_out.
DATA:g_bankn LIKE zdimwsbk-zdimuscod.             "和科目对账单对应银行账号
DATA it_bank_st TYPE TABLE OF zdimwsbk  WITH HEADER LINE.
DATA it_bsis_st TYPE TABLE OF zdimbsbs  WITH HEADER LINE.
DATA g_bldat LIKE bkpf-aedat.
DATA: g_flg_ind TYPE c,
      g_r_sel TYPE c,
      g_r_unsel TYPE c.
DATA: g_uscod(30) TYPE c,
      g_hkont like bseg-hkont.
DATA g_dmbtr TYPE p DECIMALS 2.
data: g_fieldname like dd03l-fieldname.
DATA:l_repid TYPE sy-repid.
DATA itab LIKE TABLE OF rsparams WITH HEADER LINE.
data: begin of it_zuscod occurs 0,
        BUKRS like ZDIMBUHK-bukrs,
        HKONT like ZDiMBUHK-hkont,
        TXT50 like ZDiMBUHK-txt50,
        ZDIMUSCOD like ZDiMBUHK-zdimuscod,
        uscodTXT  like ZDiMBUHK-txt50,
      end of it_zuscod.
DATA:ok_code TYPE  sy-ucomm.
TYPE-POOLS: slis.
DATA:  v_slis_field TYPE slis_selfield.
DATA:  p_alv_layout       TYPE slis_layout_alv,     "ALV 样式
       p_stru_disvar      TYPE disvariant.          "ALV 显示格式
DATA   git_events TYPE slis_t_event."不能有表头,否则会RUNTIME ERROR

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv ,
      gs_layout TYPE slis_layout_alv,
      g_repid LIKE sy-repid ,
      v_stru_disvar      TYPE disvariant .      "ALV 显示格式
DATA: control_title TYPE lvc_title.
DATA:gt_sort TYPE slis_t_sortinfo_alv.
DATA:col_pos TYPE i.
DATA:msgtxt TYPE string."输出消息变量
* 声明类
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: gt_sflight TYPE TABLE OF sflight,
      gt_sbook TYPE TABLE OF sbook,
      tt_fieldcat TYPE lvc_t_fcat,
      g_max TYPE i VALUE 100,
      gs_layout1   TYPE lvc_s_layo,
      alv_exclude TYPE ui_functions,
      alv_variant  TYPE disvariant,
      cont_on_main   TYPE scrfname VALUE 'BCALVC_TOOLBAR_D100_C1',
      cont_on_dialog TYPE scrfname VALUE 'BCALVC_TOOLBAR_D101_C1',
      grid1  TYPE REF TO cl_gui_alv_grid,
      grid2  TYPE REF TO cl_gui_alv_grid,
      custom_container1 TYPE REF TO cl_gui_custom_container,
      custom_container2 TYPE REF TO cl_gui_custom_container,
      event_receiver TYPE REF TO lcl_event_receiver.
*&---------------------------------------------------------------------*
*& 屏幕定义                                                            *
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 9002 TITLE text-011 AS WINDOW.
PARAMETERS: p_c1 AS CHECKBOX DEFAULT 'X',"added by d00194 at 090610
            p_c2 as CHECKBOX ,
            p_c3 as checkbox .
SELECTION-SCREEN END OF SCREEN 9002.
SELECTION-SCREEN BEGIN OF BLOCK xavery WITH FRAME TITLE text-001.
PARAMETERS:p_bukrs LIKE zdimwsbk-bukrs OBLIGATORY.   "公司代码
*PARAMETERS:p_hkont LIKE zdimwsbk-hkont OBLIGATORY.   "会计科目
PARAMETERS:p_zuscod LIKE ZDIMBUHK-ZDIMUSCOD OBLIGATORY.   "会计科目
PARAMETERS p_gjahr LIKE zdimwsbk-gjahr OBLIGATORY
                        DEFAULT sy-datum+0(4).   "会计年度
PARAMETERS p_monat LIKE zdimwsbk-monat OBLIGATORY
                        DEFAULT sy-datum+4(2).   "会计期间
PARAMETERS:p_bldat LIKE bkpf-aedat.           "截止日期

SELECTION-SCREEN BEGIN OF BLOCK hand WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:rd1 RADIOBUTTON GROUP rad.               "精确对账
SELECTION-SCREEN COMMENT 3(18) text-005.  "文本描述
PARAMETERS:rd2 RADIOBUTTON GROUP rad.               "模糊对账
SELECTION-SCREEN COMMENT 24(18) text-006.  "文本描述
PARAMETERS:rd3 RADIOBUTTON GROUP rad.               "手工对账
SELECTION-SCREEN COMMENT 45(18) text-007.  "文本描述
*PARAMETERS:rd4 RADIOBUTTON GROUP rad.               "冲销凭证
*SELECTION-SCREEN COMMENT 66(18) text-008.  "文本描述
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK hand.

SELECTION-SCREEN BEGIN OF BLOCK bank WITH FRAME TITLE text-010.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r1 RADIOBUTTON GROUP gr1 MODIF ID b1.     "根据会计凭证对账
SELECTION-SCREEN COMMENT 3(18) text-003 MODIF ID b1.  "文本描述
PARAMETERS: r2 RADIOBUTTON GROUP gr1 MODIF ID b1.     "根据银行账单对账
SELECTION-SCREEN COMMENT 24(16) text-004 MODIF ID b1. "
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bank.
SELECTION-SCREEN END OF BLOCK xavery.


*&---------------------------------------------------------------------*
*& 宏定义                                                              *
*&---------------------------------------------------------------------*
DEFINE m_addtext.
  text = &1.
  call method document->add_text
    EXPORTING
      text         = text
      sap_emphasis = &2.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*& 定义类                                                              *
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION
*&---------------------------------------------------------------------*
*& CLASS 实现类的方法                                                  *
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_toolbar.
* 2.In event handler method for event TOOLBAR: Append own functions
*   by using event parameter E_OBJECT.
    DATA: ls_toolbar  TYPE stb_button.
* append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.
* append an icon to show booking table
    CLEAR ls_toolbar.
    MOVE 'LOOK' TO ls_toolbar-function.
    MOVE icon_display TO ls_toolbar-icon.
    MOVE '查看会计凭证'(211) TO ls_toolbar-quickinfo.
    MOVE '查看凭证'(212) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'BOOKINGS' TO ls_toolbar-function.
    MOVE ICON_OKAY  TO ls_toolbar-icon.
    MOVE '对帐确认'(213) TO ls_toolbar-quickinfo.
    MOVE '对帐确认'(214) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
*  添加取消按钮
    CLEAR ls_toolbar.
    MOVE 'EXIT' TO ls_toolbar-function.
    MOVE ICON_CLOSE  TO ls_toolbar-icon.
    MOVE '取消'(215) TO ls_toolbar-quickinfo.
    MOVE '取消'(216) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.                    "handle_toolbar
*-------------------------------------------------------------------
  METHOD handle_user_command.
    DATA: lt_rows TYPE lvc_t_row.
    CASE e_ucomm.
      WHEN 'BOOKINGS'.
        CALL METHOD grid1->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows.
        CALL METHOD cl_gui_cfw=>flush.
        IF sy-subrc NE 0.
*        add your handling, for example
          CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
              titel = g_repid
              txt2  = sy-subrc
              txt1  = 'Error in Flush'(500).
        ELSE.
          PERFORM frm_show_booking_table TABLES lt_rows.
          LEAVE TO SCREEN 0.
        ENDIF.
      WHEN 'LOOK'.
        PERFORM frm_show_belnr TABLES lt_rows.
      WHEN 'EXIT'.
        LEAVE TO SCREEN 0.
      WHEN 'CLOSE'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.                           "handle_user_command
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

INITIALIZATION.
  select single *
    from zdimscnro
    where ZDIMSCSL eq 'Y'.
  if sy-subrc ne 0.
    message e888(sabapdocu) WITH text-114.
  else.
    select single fieldname into g_fieldname
       from dd03l
       where TABNAME = 'BSEG'
        and FIELDNAME = zdimscnro-FIELDNAME.
    if sy-subrc ne 0.
      message e888(sabapdocu) WITH text-115.
    endif.
  endif.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN                                                 *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM frm_pur_check."权限检查

at SELECTION-SCREEN on VALUE-REQUEST FOR p_ZUSCOD.
  perform FRM_GET_zdimuscod using p_ZUSCOD.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN                                                 *
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_f1_check_monat.
  PERFORM frm_f1_check_dmbtr.  "检查银行余额与SAP余额
  PERFORM frm_f1_query_bsis.
  PERFORM frm_f1_query_zdimwbsk.
  PERFORM frm_f0_compare.

end-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  frm_show_belnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
FORM frm_show_belnr  TABLES   p_lt_rows STRUCTURE lvc_s_row.
  DATA : lt_rows TYPE lvc_t_row.
  DATA : ls_rows LIKE LINE OF lt_rows.
  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  LOOP AT lt_rows INTO ls_rows.
    IF ls_rows-index <> ''.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF ls_rows-index <> ''.
    READ TABLE it_screen_out INTO wa_out INDEX ls_rows-index.
    SET PARAMETER ID 'BUK' FIELD p_bukrs.
    SET PARAMETER ID 'BLN' FIELD wa_out-belnr.
    SET PARAMETER ID 'GJR' FIELD p_gjahr.
    IF wa_out-belnr = ''.
      MESSAGE s888(sabapdocu) WITH '此项目凭证为空！'.
    ELSE.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSE.
    MESSAGE s888(sabapdocu) WITH '没有选择项目！'.
  ENDIF.

ENDFORM.                    " frm_show_belnr
*&---------------------------------------------------------------------*
*&      Form  frm_show_booking_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
FORM frm_show_booking_table  TABLES   p_et_index_rows
                                STRUCTURE lvc_s_row.

  DATA: ls_selected_line LIKE lvc_s_row,
        lf_row_index TYPE lvc_index,
        ls_sflight LIKE LINE OF gt_sflight,
        ls_index TYPE c.

  ls_index = 'X'.
  READ TABLE it_out INDEX v_slis_field-tabindex.
  PERFORM frm_check_balance TABLES p_et_index_rows.
  LOOP AT p_et_index_rows INTO ls_selected_line.
    lf_row_index = ls_selected_line-index.
    CLEAR wa_out.
    READ TABLE it_screen_out INDEX lf_row_index INTO wa_out.
    IF r1 = 'X'.
      it_out-edit = 'X'.
      it_out-gjahr1 = wa_out-gjahr1.
      it_out-monat1 = wa_out-monat1.
      it_out-zdimuscod = wa_out-zdimuscod.
      it_out-zdimcid = wa_out-zdimcid.
      it_out-zdimline = wa_out-zdimline.
      it_out-zdimname = wa_out-zdimname.
      it_out-zdimcscod = wa_out-zdimcscod.
      it_out-zdimedate = wa_out-zdimedate.
      it_out-shkzg1 = wa_out-shkzg1.
      it_out-dmbtr1 = wa_out-dmbtr1.
      it_out-zdimdtext = wa_out-zdimdtext.
    ELSEIF r2 = 'X'.
      it_out-edit = 'X'.
      it_out-bukrs = wa_out-bukrs.
      it_out-gjahr = wa_out-gjahr.
      it_out-belnr = wa_out-belnr.
      it_out-buzei = wa_out-buzei.
      it_out-budat = wa_out-budat.
      it_out-bldat = wa_out-bldat.
      it_out-monat = wa_out-monat.
      it_out-shkzg = wa_out-shkzg.
      it_out-dmbtr = wa_out-dmbtr.
      it_out-xref3 = wa_out-xref3.
      it_out-sgtxt = wa_out-sgtxt.
      it_out-hkont = wa_out-hkont.
    ENDIF.
    it_out-zdimflag = 'X'.
    IF ls_index = 'X'.
      MODIFY it_out INDEX v_slis_field-tabindex.
      CLEAR ls_index.
    ELSE.
      INSERT it_out INDEX v_slis_field-tabindex.
    ENDIF.
    v_slis_field-tabindex = v_slis_field-tabindex + 1.
  ENDLOOP.
* 更新对过帐的内表
  CLEAR lf_row_index.
  REFRESH it_out2.
  LOOP AT it_screen_out INTO wa_out.
    lf_row_index = lf_row_index + 1.
    READ TABLE p_et_index_rows INTO ls_selected_line
                               WITH KEY INDEX = lf_row_index.
    IF sy-subrc = 0.
*      APPEND wa_out TO i_out2.
    ELSE.
      APPEND wa_out TO it_out2.
    ENDIF.
  ENDLOOP.
  FREE it_screen_out.
  LOOP AT it_out2.
    APPEND it_out2 TO it_screen_out.
  ENDLOOP.
ENDFORM.                    " frm_show_booking_table
*&---------------------------------------------------------------------*
*&      Form  frm_check_balance
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ET_INDEX_ROWS  text
*----------------------------------------------------------------------*
FORM frm_check_balance  TABLES   p_et_index_rows.
  DATA: l_dmbtr1 LIKE  bsis-dmbtr.
  DATA: l_dmbtr2 LIKE bsis-dmbtr.
  DATA: ls_selected_line LIKE lvc_s_row,
        lf_row_index TYPE lvc_index,
        ls_sflight LIKE LINE OF gt_sflight,
        ls_index TYPE c.

  IF r1 = 'X'.
    l_dmbtr1 = it_out-dmbtr.
    IF it_out-shkzg = 'H'.
      l_dmbtr1 = 0 - l_dmbtr1.
    ENDIF.
  ELSE.
    l_dmbtr1 = it_out-dmbtr1.
    IF it_out-shkzg1 = 'H'.
      l_dmbtr1 = 0 - l_dmbtr1.
    ENDIF.
  ENDIF.

  LOOP AT p_et_index_rows INTO ls_selected_line.
    lf_row_index = ls_selected_line-index.
    CLEAR wa_out.
    READ TABLE it_screen_out INDEX lf_row_index INTO wa_out.
    IF r1 = 'X'.  "按会计凭证对账
      IF wa_out-shkzg1 = 'S'.
        l_dmbtr2 =  l_dmbtr2 + wa_out-dmbtr1.
      ELSEIF wa_out-shkzg1 = 'H'.
        l_dmbtr2 =  l_dmbtr2 - wa_out-dmbtr1.
      ENDIF.
    ELSEIF r2 = 'X'.
      IF wa_out-shkzg = 'S'.
        l_dmbtr2 =  l_dmbtr2 + wa_out-dmbtr.
      ELSEIF wa_out-shkzg = 'H'.
        l_dmbtr2 =  l_dmbtr2 - wa_out-dmbtr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  l_dmbtr2 = 0 - l_dmbtr2.
  IF l_dmbtr1 NE l_dmbtr2.
    MESSAGE s888(sabapdocu) WITH  'ERROR:金额不匹配，不能对账！'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " frm_check_balance
*&---------------------------------------------------------------------*
*&      Form  frm_pur_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_pur_check .
  DATA : i_msg TYPE string.
  DATA: BEGIN OF i_bukrs OCCURS 0,
          bukrs LIKE t001k-bukrs,
          werks LIKE t001k-bwkey,"评估范围
       END OF i_bukrs.
  DATA:g_str TYPE string.
  select single hkont into g_hkont
    from ZDIMBUHK
    where bukrs = p_bukrs
      and ZDIMUSCOD = p_ZUSCOD.
  if sy-subrc ne 0.
    MESSAGE e888(sabapdocu) WITH text-111.
  endif.
*  AUTHORITY-CHECK OBJECT 'Z_FI_BANK' ID 'BUKRS' FIELD p_bukrs
*                                     ID 'SAKNR' FIELD p_hkont.
*  IF sy-subrc <> 0.
*    CONCATENATE '你没有公司代码' p_bukrs
*                '及其对应科目' p_hkont '的权限！' INTO I_msg.
*    MESSAGE e888(sabapdocu) WITH i_msg.
*    STOP.
*  ENDIF.
*  SELECT bukrs
**         bwkey AS werks
*  INTO TABLE i_bukrs
*  FROM t001
*  WHERE bukrs = p_bukrs.
*  IF LINES( i_bukrs ) = 0.
*    MESSAGE '请重新选择条件，不存在公司条件' TYPE 'E'.
*    STOP.
*  ELSE.
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
*        MESSAGE g_str TYPE 'E'.
*        STOP.
*      ENDIF.
*    ENDLOOP.
*  endif.
ENDFORM.                    " frm_pur_check
*&---------------------------------------------------------------------*
*&  FORM frm_f1_check_monat.
*&---------------------------------------------------------------------*
*&  检查当前的期间是否可用。
*&---------------------------------------------------------------------*
FORM frm_f1_check_monat.
  DATA l_monat LIKE bsis-monat.
  DATA l_gjahr LIKE bsis-gjahr.
  DATA i_monat TYPE TABLE OF zdimblock WITH HEADER LINE.
  IF p_monat = '01'.
    l_monat = '12'.
    l_gjahr = p_gjahr - 1.
    SELECT * FROM zdimblock INTO TABLE i_monat
      WHERE bukrs = p_bukrs  AND
            gjahr = p_gjahr  AND
            hkont = g_hkont  AND
            ZDIMUSCOD = p_zuscod and
            monat = p_monat.

    SELECT * FROM zdimblock INTO TABLE i_monat
      WHERE bukrs = p_bukrs  AND
            gjahr = l_gjahr  AND
            hkont = g_hkont  AND
            ZDIMUSCOD = p_zuscod and
            monat = l_monat.
  ELSE.
    l_monat = p_monat - 1.
    REFRESH i_monat.
    SELECT * FROM zdimblock INTO TABLE i_monat
      WHERE bukrs = p_bukrs  AND
            gjahr = p_gjahr  AND
            hkont = g_hkont  AND
            ZDIMUSCOD = p_zuscod and
            ( monat = l_monat OR monat = p_monat ).
  ENDIF.
  IF p_bldat IS INITIAL.
    g_bldat+0(4) = p_gjahr.
    g_bldat+4(2) = p_monat.
    g_bldat+6(2) = '01'.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = g_bldat
      IMPORTING
        last_day_of_month = g_bldat
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
  ELSE.
    g_bldat = p_bldat.
  ENDIF.

  IF sy-subrc = 0.
    LOOP AT i_monat.
      IF i_monat-monat = l_monat.
        IF i_monat-zdimlock NE 'X'.
          MESSAGE i888(sabapdocu) WITH '上一期间未关闭，本期间未打开！'.
          STOP.
        ENDIF.
      ELSEIF i_monat-monat = p_monat.
        IF i_monat-zdimlock = 'X'.
          MESSAGE i888(sabapdocu) WITH '当前期间已关闭！'.
          STOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.   "第一次对帐
    SELECT SINGLE * FROM zdimblock
         WHERE bukrs = p_bukrs AND
               hkont = g_hkont and
               ZDIMUSCOD = p_zuscod.
    IF sy-subrc = 0.
      MESSAGE i888(sabapdocu) WITH '请选择正确的期间！'.
    ELSE.
    ENDIF.
  ENDIF.
ENDFORM.                    "frm_f1_check_monat
*&---------------------------------------------------------------------*
*&      Form  f1_check_dmbtr
*&---------------------------------------------------------------------*
*       检查SAP余额与银行余额是否一值。
*----------------------------------------------------------------------*
FORM frm_f1_check_dmbtr.
*  DATA: l_zpglt0 LIKE TABLE OF zpglt0 WITH HEADER LINE.
*  DATA: l_glt0 LIKE TABLE OF glt0 WITH HEADER LINE.
*  DATA l_bank LIKE glt0-hslvt.
*  DATA l_sap LIKE glt0-hslvt.
*  DATA l_item LIKE zpwseg-dmbtr.
**  data l_item like
*  SELECT * FROM zpwsbk
*       WHERE bukrs = p_bukrs AND
*             hkont = p_hkont AND
*             gjahr = '2008' AND
*             monat = '04'.
*   IF zpwseg-shkzg = 'S'.
*     l_item = l_item + zpwsbk-dmbtr.
*   ELSE.
*     l_item = l_item - zpwsbk-dmbtr.
*   ENDIF.
*  ENDSELECT.
*
*  SELECT * FROM zpglt0  INTO TABLE l_zpglt0
*       WHERE bukrs = p_bukrs AND
*             hkont = p_hkont AND
*             gjahr = '2008'.
*   LOOP AT l_zpglt0 WHERE shkzg = 'H'.
*       ADD l_zpglt0-hslvt THEN l_zpglt0-hsl01
*       UNTIL l_zpglt0-hsl04 GIVING l_bank.
*   ENDLOOP.
*   l_bank = 0 - l_bank.
*   LOOP AT l_zpglt0 WHERE  shkzg = 'S'.
*     ADD l_zpglt0-hslvt THEN l_zpglt0-hsl01
*     UNTIL l_zpglt0-hsl04 TO l_bank.
*   ENDLOOP.
*
*   SELECT * FROM glt0 INTO TABLE l_glt0
*        WHERE bukrs = p_bukrs AND
*              racct = p_hkont AND
*              ryear = '2008'.
*   LOOP AT l_glt0.
*       ADD l_glt0-hslvt THEN l_glt0-hsl01 UNTIL l_glt0-hsl04
*       TO l_sap.
*   ENDLOOP.
*  FREE: l_zpglt0, l_glt0.
*  l_bank = l_bank + l_item.
*  l_bank = 0 - l_bank.
*  SELECT SINGLE * FROM zdimbuhk
*       WHERE bukrs = p_bukrs AND
*             hkont = p_hkont.
*
*  IF sy-subrc = 0 AND zdimbuhk-zdimflag = 'X'.
**  IF l_sap NE l_bank.
*    MESSAGE  i888(sabapdocu) WITH
*    '银行余额与SAP系统余额不一致，请查看余额调节表！'.
*    STOP.
*  ENDIF.
ENDFORM.                    " frm_f1_check_dmbtr
*&---------------------------------------------------------------------*
*& FORM FRM_F1_QUERY_BSIS.
*&---------------------------------------------------------------------*
*& 取尚未对过帐的会计凭证
*&---------------------------------------------------------------------*
FORM frm_f1_query_bsis.
  data: i_flag,
      I_char1(100) ,
      i_char2(150),
      i_char(30).
  DATA: d_bkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE,
        it_bseg like table of bseg with header line.
  FIELD-SYMBOLS: <fs_bsis> like it_bsis,
                 <fs_char> type any.
  CLEAR iT_bsis.
*  select BUKRS
*         BELNR
*         GJAHR
*         budat
*         bldat
*         monat
*         waers
*  into CORRESPONDING FIELDS OF TABLE d_bkpf
*  from bkpf
*  WHERE bukrs = p_bukrs AND
*        gjahr = p_gjahr AND
*        monat = p_monat and
*        stblg eq space  AND
*        stjah eq space.
*if sy-subrc eq 0.
* sort d_bkpf by bukrs
*                BELNR
*                GJAHR.
*
*  SELECT bukrs
*        hkont
*        zz0001
*        gjahr
*        belnr
*        buzei
*        shkzg
*        dmbtr
*        xref3
*        sgtxt
*      FROM bseg
*      INTO CORRESPONDING FIELDS OF TABLE iT_bsis
*      for ALL ENTRIES IN d_bkpf
*      WHERE BUKRS = d_bkpf-bukrs
*        and BELNR = d_bkpf-belnr
*        and GJAHR = d_bkpf-gjahr
*        and hkont = g_hkont
*        AND zz0001  = p_zuscod .
*
* loop at it_bsis assigning <fs_bsis>.
*   read table d_bkpf with key  BUKRS = <fs_bsis>-bukrs
*                               BELNR = <fs_bsis>-belnr
*                               GJAHR = <fs_bsis>-gjahr
*                               BINARY SEARCH.
*  if sy-subrc eq 0.
*         <fs_bsis>-budat = d_bkpf-budat.
*         <fs_bsis>-bldat = d_bkpf-bldat.
*         <fs_bsis>-monat = d_bkpf-monat.
*         <fs_bsis>-waers = d_bkpf-waers.
*  endif.
* endloop.
*endif.
*检查科目货币与本位币是否一致
  clear: it_bsis[],
         it_bsis.
  clear: t001,
         skb1.
  select single *
    from t001
    where bukrs = p_bukrs.
  select single *
    from skb1
    where bukrs = p_bukrs
      and SAKNR = g_hkont.
  if t001-waers eq skb1-waers.
    i_flag = 'X'.
  else.
    clear  i_flag.
  endif.
  select single *
    from zdimldgrp
    where bukrs = p_bukrs.
  CLEAR iT_bsis.
  select BUKRS
         BELNR
         GJAHR
         budat
         bldat
         monat
         waers
  into CORRESPONDING FIELDS OF TABLE d_bkpf
  from bkpf
  WHERE bukrs = p_bukrs AND
        gjahr = p_gjahr AND
        monat = p_monat and
        stblg eq space  AND
        stjah eq space  .
*    and RLDNR eq zdimldgrp-RLDNR.
  if sy-subrc eq 0.
    sort d_bkpf by bukrs
                   BELNR
                   GJAHR.
    CONCATENATE 'IT_BSEG-' g_fieldname into i_char.
    ASSIGN  (i_char) to <fs_char>.
    if i_flag eq 'X'.   "当本位币与业务货币一样时取本位币金额
*      SELECT *
*          FROM bseg
*          INTO CORRESPONDING FIELDS OF TABLE it_bseg
*          for ALL ENTRIES IN d_bkpf
*          WHERE BUKRS = d_bkpf-bukrs
*            and BELNR = d_bkpf-belnr
*            and GJAHR = d_bkpf-gjahr
*            and hkont = g_hkont
*           AND zz0001  eq  i_zz0001.
      CONCATENATE 'bukrs hkont gjahr belnr buzei shkzg dmbtr xref3 sgtxt'  g_fieldname into i_char1 SEPARATED BY space.
      CONCATENATE  'BUKRS = d_bkpf-bukrs and BELNR = d_bkpf-belnr and GJAHR = d_bkpf-gjahr and hkont = g_hkont and'  g_fieldname  ' = p_zuscod.' into i_char2 SEPARATED BY space.
      select (i_char1)
        into CORRESPONDING FIELDS OF TABLE it_bseg
        from bseg
         for ALL ENTRIES IN d_bkpf
         where (i_char2).

      loop at it_bseg .
        read table d_bkpf with key  BUKRS = it_bseg-bukrs
                                    BELNR = it_bseg-belnr
                                    GJAHR = it_bseg-gjahr
                                    BINARY SEARCH.
        if sy-subrc eq 0.
          it_bsis-bukrs = it_bseg-bukrs.
          it_bsis-hkont = it_bseg-hkont.
          it_bsis-zdimuscod = <fs_char>.
          it_bsis-gjahr = it_bseg-gjahr.
          it_bsis-belnr = it_bseg-belnr.
          it_bsis-buzei = it_bseg-buzei.
          it_bsis-shkzg = it_bseg-shkzg.
          it_bsis-dmbtr = it_bseg-dmbtr.
          it_bsis-xref3 = it_bseg-xref3.
          it_bsis-sgtxt = it_bseg-sgtxt.
          it_bsis-budat = d_bkpf-budat.
          it_bsis-bldat = d_bkpf-bldat.
          it_bsis-monat = d_bkpf-monat.
          it_bsis-waers = t001-waers.
          append it_bsis.
          clear it_bsis.
        endif.
      endloop.
    else.                     "当本位币不业务货币一样时金额取业务货币金额

*      SELECT *
*            FROM bseg
*            INTO CORRESPONDING FIELDS OF TABLE it_bseg
*            for ALL ENTRIES IN d_bkpf
*            WHERE BUKRS = d_bkpf-bukrs
*              and BELNR = d_bkpf-belnr
*              and GJAHR = d_bkpf-gjahr
*              and hkont = g_hkont
*             AND zz0001  eq  i_zz0001.
      CONCATENATE 'bukrs hkont gjahr belnr buzei shkzg wrbtr as dmbtr xref3 sgtxt'  g_fieldname into i_char1 SEPARATED BY space.
      CONCATENATE  'BUKRS = d_bkpf-bukrs and BELNR = d_bkpf-belnr and GJAHR = d_bkpf-gjahr and hkont = g_hkont and'  g_fieldname  ' = p_zuscod.' into i_char2 SEPARATED BY space.
      select (i_char1)
        into CORRESPONDING FIELDS OF TABLE it_bseg
        from bseg
         for ALL ENTRIES IN d_bkpf
         where (i_char2).
      loop at it_bseg .
        read table d_bkpf with key  BUKRS = it_bseg-bukrs
                                    BELNR = it_bseg-belnr
                                    GJAHR = it_bseg-gjahr
                                    BINARY SEARCH.
        if sy-subrc eq 0.
          it_bsis-bukrs = it_bseg-bukrs.
          it_bsis-hkont = it_bseg-hkont.
          it_bsis-zdimuscod = <fs_char>.
          it_bsis-gjahr = it_bseg-gjahr.
          it_bsis-belnr = it_bseg-belnr.
          it_bsis-buzei = it_bseg-buzei.
          it_bsis-shkzg = it_bseg-shkzg.
          it_bsis-dmbtr = it_bseg-dmbtr.
          it_bsis-xref3 = it_bseg-xref3.
          it_bsis-sgtxt = it_bseg-sgtxt.
          it_bsis-budat = d_bkpf-budat.
          it_bsis-bldat = d_bkpf-bldat.
          it_bsis-monat = d_bkpf-monat.
          it_bsis-waers = d_bkpf-waers.
          append it_bsis.
          clear it_bsis.
        endif.
      endloop.
    endif.
  endif.
  clear: d_bkpf[],
         d_bkpf.
*过滤冲销凭证
  SELECT * FROM bkpf INTO CORRESPONDING FIELDS OF TABLE d_bkpf
    WHERE bukrs = p_bukrs
    AND   gjahr = p_gjahr
    AND   stblg <> ''
    AND   stjah <> ''.
  loop at d_bkpf .
    DELETE FROM zdimbsbs WHERE bukrs = it_bsis-bukrs
                        AND gjahr = d_bkpf-stjah
                        AND belnr = d_bkpf-stblg
                        and ZDIMFLAG ne 'X'.

  ENDLOOP.
* 取当前月已对过帐的会计凭证
*  SELECT * FROM zdimbsbs
*       INTO CORRESPONDING FIELDS OF TABLE it_bsas
*       WHERE bukrs = p_bukrs AND
*             hkont = p_hkont AND
*             gjahr = p_gjahr AND
*             monat = p_monat AND
*             zdimflag = 'X'.
** 删除已对过帐的会计凭证
*  SORT it_bsas BY bukrs hkont gjahr belnr buzei.
*  LOOP AT it_bsis.
*    READ TABLE it_bsas WITH KEY gjahr = it_bsis-gjahr
*                               belnr = it_bsis-belnr
*                               buzei = it_bsis-buzei
*                               BINARY SEARCH.
*    IF sy-subrc = 0.
*      DELETE it_bsis.    "删除已对过帐的会计凭证
*    ENDIF.
*  ENDLOOP.
*  PERFORM f_update_zbsbs.
* 取未对过帐的以前月与当前月的会计凭证
  clear: it_bsis[],
         it_bsis.
  SELECT * FROM zdimbsbs
       APPENDING CORRESPONDING FIELDS OF TABLE it_bsis
       WHERE bukrs = p_bukrs AND
             hkont = g_hkont AND
             ZDIMUSCOD = p_zuscod and
             gjahr = p_gjahr AND
             monat LE p_monat AND
             zdimflag NE 'X'.
* 取未对过帐的以前月的会计凭证：跨年凭证
  SELECT * FROM zdimbsbs APPENDING CORRESPONDING FIELDS OF TABLE it_bsis
        WHERE bukrs = p_bukrs AND
              hkont = g_hkont AND
              ZDIMUSCOD = p_zuscod and
              gjahr LT p_gjahr AND
              zdimflag NE 'X'.
ENDFORM.                    " frm_f1_query_bsis
*&---------------------------------------------------------------------*
*& FORM FRM_F1_QUERY_ZPWBSK
*&---------------------------------------------------------------------*
*& 取未对过帐的银行对帐单
*&---------------------------------------------------------------------*
FORM frm_f1_query_zdimwbsk.
*  CLEAR g_bankn.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*       EXPORTING
*            input  = g_hkont
*       IMPORTING
*            output = g_hkont.
*  SELECT SINGLE zdimuscod INTO g_bankn FROM zdimbuhk
*      WHERE bukrs = p_bukrs AND
*            hkont = g_hkont.

  SELECT bukrs
         zdimuscod
         gjahr
         monat
         zdimcid
         zdimline
         zdimname
         zdimcscod
         zdimedate
         shkzg
         dmbtr
         zdimdtext
       FROM zdimwsbk
       INTO CORRESPONDING FIELDS OF TABLE it_bank
       WHERE bukrs = p_bukrs AND
             zdimuscod = p_zuscod AND
             zdimflag NE 'X'.
  g_uscod = p_zuscod.
ENDFORM.                    "frm_f1_query_zdimwbsk
*&---------------------------------------------------------------------*
*&      Form  frm_f0_compare
*&---------------------------------------------------------------------*
*       对帐准备工作
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f0_compare .
  IF rd1 = 'X'.      "精确对帐
    CALL SELECTION-SCREEN '9002' STARTING AT 10 10.
    PERFORM frm_f1_compare_1.
    PERFORM frm_f_output.
  ELSEIF rd2 = 'X'.  "模糊对帐
    PERFORM frm_f1_compare_2.
    PERFORM frm_f_output.
  ELSEIF rd3 = 'X'.  "手工对帐
    PERFORM frm_f1_compare_3.
    PERFORM frm_f_output.
  ENDIF.
ENDFORM.                    " frm_f0_compare
*&---------------------------------------------------------------------*
*&      Form  frm_f1_compare_1
*&---------------------------------------------------------------------*
*        精确对帐
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f1_compare_1 .
  DATA l_shkzg TYPE c.
  ranges: s_xref3 for bsis-xref3,
          s_bldat for bkpf-bldat,
          s_ZDIMDTEXT for zdimwsbk-ZDIMDTEXT.

*  SORT i_bank BY shkzg dmbtr cscod ddate .

  LOOP AT it_bsis WHERE xref3 <> '' and  xref3 <> '*'.
    IF it_bsis-shkzg = 'S'.
      l_shkzg = 'H'.
    ELSEIF it_bsis-shkzg = 'H'.
      l_shkzg = 'S'.
    ENDIF.
    CLEAR it_bank.
  clear: s_bldat[],
         s_bldat,
         s_xref3[],
         s_xref3,
         s_ZDIMDTEXT[],
         s_ZDIMDTEXT.

  IF p_c1 IS NOT INITIAL."added by d00194 at 090531
    s_bldat-SIGN = 'I'.
    s_bldat-option = 'EQ'.
    s_bldat-low  = it_bsis-bldat.
    append s_bldat.
    clear s_bldat.
  ENDIF.

  IF p_c2 IS NOT INITIAL.""added by d00194 at 090531
    s_xref3-sign = 'I'.
    s_xref3-option = 'EQ'.
    s_xref3-low = it_bsis-xref3.
    "changed by d00194 at 090526
    APPEND s_xref3.
    CLEAR s_xref3.
*    append s_bldat.
*    clear s_bldat.
  ENDIF.

  IF p_c3 IS NOT INITIAL."added by d00194 at 090531
    s_ZDIMDTEXT-sign = 'I'.
    s_ZDIMDTEXT-option = 'EQ'.
    s_ZDIMDTEXT-low = it_bsis-sgtxt.
    append s_ZDIMDTEXT.
    clear s_ZDIMDTEXT.
  ENDIF.
    LOOP AT it_bank WHERE
                          shkzg = l_shkzg
                      AND dmbtr = it_bsis-dmbtr
                      AND zdimedate in s_bldat
                      AND zdimcscod in s_xref3
                      and ZDIMDTEXT in s_ZDIMDTEXT.
                      .
      PERFORM frm_f1_compare_table.
      DELETE it_bank.
      EXIT.
    ENDLOOP.
*      READ TABLE i_bank WITH KEY shkzg = l_shkzg
*                                 dmbtr = i_bsis-dmbtr
*                                 cscod = i_bsis-xref3
*                                 ddate = i_bsis-bldat
**                                 dtext = i_bsis-sgtxt
*                                 BINARY SEARCH.
*
*      IF sy-subrc = 0.
*        PERFORM f1_compare_table.
*      ENDIF.
  ENDLOOP.
ENDFORM.                    " frm_f1_compare_1
*&---------------------------------------------------------------------*
*&      Form  frm_f1_compare_table
*&---------------------------------------------------------------------*
*       更新内表
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f1_compare_table .
  CLEAR it_out.
  it_out-gjahr = it_bsis-gjahr.
  it_out-monat = it_bsis-monat.
  it_out-bukrs = it_bsis-bukrs.
  it_out-budat = it_bsis-budat.
  it_out-bldat = it_bsis-bldat.
  it_out-belnr = it_bsis-belnr.
  it_out-buzei = it_bsis-buzei.
  it_out-hkont = it_bsis-hkont.
  it_out-waers = it_bsis-waers.
  it_out-shkzg = it_bsis-shkzg.
  it_out-sgtxt = it_bsis-sgtxt.
  it_out-dmbtr = it_bsis-dmbtr.
  it_out-xref3 = it_bsis-xref3.
  it_out-zdimcid = it_bank-zdimcid.
  it_out-zdimline = it_bank-zdimline.
  it_out-gjahr1 = it_bank-gjahr.
  it_out-monat1 = it_bank-monat.
  it_out-zdimname = it_bank-zdimname.
  it_out-zdimcscod = it_bank-zdimcscod.
  it_out-zdimedate = it_bank-zdimedate.
  it_out-shkzg1 = it_bank-shkzg.
  it_out-dmbtr1 = it_bank-dmbtr.
  it_out-zdimdtext = it_bank-zdimdtext.
  IF rd3 NE 'X'.
    it_out-zdimflag = 'X'.
  ENDIF.
  it_out-zdimuscod = g_uscod.
  APPEND it_out.
ENDFORM.                    " frm_f1_compare_table
*&---------------------------------------------------------------------*
*&      Form  frm_f_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f_output .
  DATA:l_repid TYPE sy-repid.
  l_repid = sy-repid.
*   ALV 设置
  p_alv_layout-zebra             = 'X'.
  p_alv_layout-totals_text       = 'X' .
  p_alv_layout-box_tabname            = 'ITAB'.
*   设置ALV FIELD
  PERFORM fieldcat.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program          = l_repid
            is_layout                   = p_alv_layout
            it_fieldcat                 = gt_fieldcat
            it_sort                     = gt_sort[]
            i_grid_title                = control_title
            i_save                      = 'A'
            i_callback_user_command     = 'FRM_UCOMM'  "实现双击事件
            is_variant                  = p_stru_disvar
            i_callback_pf_status_set    = 'SET_STATUS'
*            i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
            it_events                   = git_events[]
       TABLES
            t_outtab                    = it_out  "输出的内表
       EXCEPTIONS
            program_error               = 1
            OTHERS                      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " frm_f_output
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fieldcat.
  PERFORM e01_fieldcat_init USING :
    'EDIT'  '选择'       4  'X' ''  ''  'X'  '' ' '      '' 'X'.
  if r2 eq 'X' and rd3 eq 'X'.
    PERFORM e01_fieldcat_init USING :
      'ZDIMCID' '银行流水号' 10  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
       'ZDIMLINE' '项目号'    6   '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
       'ZDIMNAME' '对方用户名' 12  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
       'ZDIMCSCOD' '对方帐号'   20  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
       'ZDIMEDATE' '交易日期'   10  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
       'SHKZG1' '借贷标识'  8   '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
       'DMBTR1' '银行金额'  13  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
       'ZDIMDTEXT' '摘要'       20  '' 'X' ''  ''  ' ' 'ZDIMWEG' '' ''.
  endif.
  PERFORM e01_fieldcat_init USING :
      'BUKRS' '公司代码'   8   '' 'X'  ''  '' ' ' ''        '' '',
      'BUDAT' '记帐日期'   12  '' 'X' ''  ''  ' ' 'BKPF'   '' '',
      'BLDAT' '凭证日期'   12  '' 'X' ''  ''  ' ' 'BKPF'   '' '',
      'BELNR' '会计凭证'   10  '' 'X' ''  ''  ' ' 'BSIS'   '' '',
      'BUZEI' '凭证行项目' 10  '' 'X' ''  ''  ' ' 'BSIS'   '' '',
      'HKONT' '会计科目'   10  '' 'X' ''  ''  ' ' 'BSIS'   '' '',
      'WAERS' '货币'       4   '' 'X' ''  ''  ' ' 'BSIS'   '' '',
      'SHKZG' '借贷标记'   8   '' 'X' ''  ''  ' ' 'BSIS'   '' '',
      'SGTXT' '会计凭证摘要' 20 '' 'X' '' ''  ' ' 'BSIS'   '' '',
      'DMBTR' '金额'       13  '' 'X' ''  ''  ' ' 'BSIS'   '' '',
      'XREF3' '参考码3'    20  '' 'X' ''  ''  ' ' 'BSIS'   '' ''.
  if r2 ne 'X' or rd3 ne 'X'.
    PERFORM e01_fieldcat_init USING :
   'ZDIMCID' '银行流水号' 10  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
   'ZDIMLINE' '项目号'    6   '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
   'ZDIMNAME' '对方用户名' 12  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
   'ZDIMCSCOD' '对方帐号'   20  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
   'ZDIMEDATE' '交易日期'   10  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
   'SHKZG1' '借贷标识'  8   '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
   'DMBTR1' '银行金额'  13  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
   'ZDIMDTEXT' '摘要'       20  '' 'X' ''  ''  ' ' 'ZDIMWEG' '' ''.
  endif.
  PERFORM e01_fieldcat_init USING :
  'ZDIMFLAG' '对帐完成标识' 12  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' ''.
ENDFORM.                    "FIELDCAT
*&---------------------------------------------------------------------*
*& FORM E01_fieldcat_init.
*&---------------------------------------------------------------------*
FORM e01_fieldcat_init USING field_name      TYPE c
                             field_text      TYPE c
                             field_lenth     TYPE i
                             field_type      TYPE c
                             field_key       TYPE c
                             field_hotspot   TYPE c
                             field_checkbox  TYPE c
                             field_no_zero   TYPE c
                             field_ref_tabname TYPE c
                             field_emphasize TYPE c
  field_input TYPE c.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  col_pos = col_pos + 1.
  ls_fieldcat-col_pos = col_pos.
  ls_fieldcat-fieldname = field_name.
  ls_fieldcat-seltext_l = field_text.
  ls_fieldcat-seltext_m = field_text.
  ls_fieldcat-seltext_s = field_text.
  ls_fieldcat-checkbox  = field_checkbox.
  ls_fieldcat-input = field_input.
  IF field_name = 'EDIT' AND ( rd1 = 'X' OR rd2 = 'X'
                         OR ( rd3 = 'X' AND r1 = 'X' ) ).
    ls_fieldcat-edit = 'X'.
  ENDIF.
*  ls_fieldcat-round = 1.
  IF field_type = 'Q'.
    ls_fieldcat-just     = 'R'.
    ls_fieldcat-datatype = 'QUAN'.
  ELSE.
    ls_fieldcat-just = 'L'.
  ENDIF.
  ls_fieldcat-key  = field_key.
  ls_fieldcat-hotspot = field_hotspot .
  ls_fieldcat-outputlen = field_lenth.
  ls_fieldcat-no_zero = field_no_zero.
  ls_fieldcat-emphasize = field_emphasize .
  ls_fieldcat-ref_tabname = field_ref_tabname.
  APPEND ls_fieldcat TO gt_fieldcat.
ENDFORM.                    " e01_fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  FRM_UCOMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_ucomm USING f_ucomm    LIKE sy-ucomm
                     f_selfield TYPE slis_selfield.
  DATA:l_flag(1) .
  CLEAR msgtxt.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  CALL METHOD lr_grid->check_changed_data.
  f_selfield-refresh = 'X'.
  CLEAR v_slis_field.
  v_slis_field = f_selfield.
  IF f_ucomm = '&IC1'.
    CLEAR it_out.
    READ TABLE it_out INDEX v_slis_field-tabindex.
    IF it_out-zdimflag = 'X'.
      MESSAGE e888(sabapdocu) WITH '该项目已对帐！'.
      EXIT.
    ENDIF.
    IF rd3 = 'X'.
      CALL SCREEN 9000 STARTING AT 10 50.
    ENDIF.
  ENDIF.
  CASE f_ucomm.
    WHEN 'APP' .
      "添加记录功能不予支持，防止用户随便对帐
    WHEN 'SAVE'.
      PERFORM f2_deal_table.
      PERFORM frm_f_answer.
    WHEN  'DEL' ."删除选择行
      PERFORM f2_delevent.
    WHEN 'ALL'.
      LOOP AT it_out.
        it_out-edit = 'X'.
        MODIFY it_out.
      ENDLOOP.
    WHEN 'SAL'.
      LOOP AT it_out.
        it_out-edit = ''.
        MODIFY it_out.
      ENDLOOP.
    WHEN 'CLEAR'.
      PERFORM frm_cancel_belnr.
    WHEN 'LKB' OR 'LOOK'.
      PERFORM frm_look_belnr.
    WHEN 'FILTER'.
      CALL SCREEN 9001 STARTING AT 50 8 ENDING AT 72 12.
*    WHEN 'DMT'.
*      call transaction 'ZFIR04' using itab.
  ENDCASE.
ENDFORM.                    "FRM_UCOMM

*&---------------------------------------------------------------------*
*&      Form  SET_STATUS
*&---------------------------------------------------------------------*
*       设置状态栏
*----------------------------------------------------------------------*
*       --> RT_EXTAB
*----------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab .
  IF rd3 = 'X' AND r1 = 'X'.
    SET PF-STATUS 'WITH_BELNR'.
  ELSE.
    SET PF-STATUS 'ZSTANDARD_FULLSCREEN' .         "EXCLUDING RT_EXTAB.
  ENDIF.
ENDFORM.                               " SET_STATUS2
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_get_event .
  DATA: formname_top_of_page TYPE slis_formname
                             VALUE 'FRM_TOP_OF_PAGE1',
        formname_end_of_list TYPE slis_formname VALUE 'FRM_END_OF_LIST'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = git_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "FRM_GET_EVENT
*&---------------------------------------------------------------------*
*&      Form  f2_delevent
*&---------------------------------------------------------------------*
*       删除选中的行,用户可以对精确对帐、模糊对帐的记录进行删减
*----------------------------------------------------------------------*
FORM f2_delevent.
  LOOP AT it_out.
    IF it_out-edit = 'X'.
      DELETE it_out.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " delevent
*&---------------------------------------------------------------------*
*&      Form  f2_deal_table
*&---------------------------------------------------------------------*
*       处理内表，以更新到数据库
*----------------------------------------------------------------------*
FORM f2_deal_table.
  DATA it_zdimwsbk_tmp TYPE TABLE OF zdimwsbk  WITH HEADER LINE.
  DATA it_zdimbsbs_tmp TYPE TABLE OF zdimbsbs  WITH HEADER LINE.
  FIELD-SYMBOLS: <fs_zdimwsbk> like it_zdimwsbk_tmp,
                 <fs_zdimbsbs> like it_zdimbsbs_tmp.
  REFRESH: it_bsis_st, it_bank_st.
  LOOP AT it_out WHERE zdimflag = 'X' AND edit = 'X'.
    CLEAR: it_bsis_st,it_bank_st.
    it_bsis_st-mandt  = it_bank_st-mandt  = sy-mandt.
    it_bsis_st-bukrs  = it_bank_st-bukrs  = it_out-bukrs.
    it_bsis_st-gjahr1  = it_bank_st-gjahr1 = p_gjahr.  "对帐年度
    it_bsis_st-monat1 = it_bank_st-monat1 = p_monat.   "对帐期间
    it_bsis_st-gjahr  =  it_out-gjahr.                 "会计年度
    it_bsis_st-monat  =  it_out-monat.                 "会计期间
    it_bank_st-gjahr =  it_out-gjahr1.                 "银行年度
    it_bank_st-monat  = it_out-monat1.                 "银行期间
    it_bsis_st-belnr  = it_bank_st-belnr  = it_out-belnr.
    it_bsis_st-buzei  = it_bank_st-buzei  = it_out-buzei.
    it_bsis_st-shkzg  = it_out-shkzg.
    it_bank_st-shkzg = it_out-shkzg1.
    it_bsis_st-dmbtr  = it_out-dmbtr.
    it_bank_st-dmbtr  = it_out-dmbtr1.
    it_bsis_st-zdimcid  = it_bank_st-zdimcid  = it_out-zdimcid.
    it_bsis_st-zdimline = it_bank_st-zdimline = it_out-zdimline.
    it_bsis_st-zdimuscod  = it_bank_st-zdimuscod  = it_out-zdimuscod.
    it_bsis_st-hkont = it_bank_st-hkont = g_hkont.
    it_bsis_st-zdimflag  = it_bank_st-zdimflag  = it_out-zdimflag.
    it_bsis_st-sgtxt = it_out-sgtxt.
    it_bsis_st-bldat = it_out-bldat.
    it_bank_st-zdimname  = it_out-zdimname.
    it_bank_st-zdimcscod  = it_out-zdimcscod.
    it_bank_st-zdimedate  = it_out-zdimedate.
    it_bank_st-zdimdtext  = it_out-zdimdtext.
    IF rd1 = 'X'.
      it_bsis_st-zdimtype = it_bank_st-zdimtype = '1'.
    ELSEIF rd2 = 'X'.
      it_bsis_st-zdimtype = it_bank_st-zdimtype = '2'.
    ELSE.
      it_bsis_st-zdimtype = it_bank_st-zdimtype = '3'.
      IF r1 = 'X'.      "根据会计凭证对账
        it_bank_st-zdimindie = 'X'.
        CLEAR: it_bsis_st-zdimline, it_bsis_st-zdimcid.
      ELSEIF r2 = 'X'.  "根据银行对账单对账
        it_bsis_st-zdimindie = 'X'.
        CLEAR: it_bank_st-belnr, it_bank_st-buzei.
      ENDIF.
    ENDIF.
    APPEND it_bsis_st.
    APPEND it_bank_st.
  ENDLOOP.

*  PERFORM f_lock_zbsbs.
*  PERFORM f_lock_zpwsbk.
  if not it_bsis_st[] is initial.
    select * into table it_zdimbsbs_tmp
      from zdimbsbs
      FOR ALL ENTRIES IN it_bsis_st
      where BUKRS = it_bsis_st-BUKRS
        and HKONT = it_bsis_st-HKONT
        and GJAHR = it_bsis_st-GJAHR
        and BELNR = it_bsis_st-BELNR
        and BUZEI = it_bsis_st-BUZEI.
    loop at it_zdimbsbs_tmp ASSIGNING <fs_zdimbsbs>.
      read table it_bsis_st with key BUKRS = <fs_zdimbsbs>-BUKRS
                                     HKONT = <fs_zdimbsbs>-HKONT
                                     GJAHR = <fs_zdimbsbs>-GJAHR
                                     BELNR = <fs_zdimbsbs>-BELNR
                                     BUZEI = <fs_zdimbsbs>-BUZEI.
      if sy-subrc eq 0.
        <fs_zdimbsbs>-GJAHR1 =  it_bsis_st-GJAHR1.
        <fs_zdimbsbs>-MONAT1 =  it_bsis_st-MONAT1.
        <fs_zdimbsbs>-ZDIMCID =  it_bsis_st-ZDIMCID.
        <fs_zdimbsbs>-ZDIMLINE =  it_bsis_st-ZDIMLINE.
        <fs_zdimbsbs>-ZDIMTYPE =  it_bsis_st-ZDIMTYPE.
        <fs_zdimbsbs>-ZDIMFLAG =  it_bsis_st-ZDIMFLAG.
        <fs_zdimbsbs>-ZDIMINDIE =  it_bsis_st-ZDIMINDIE.
        <fs_zdimbsbs>-ZDIMCABEL =  it_bsis_st-ZDIMCABEL.
      endif.
    endloop.
  endif.
  if not it_bank_st[] is initial.
    select * into table it_zdimwsbk_tmp
     from zdimwsbk
     FOR ALL ENTRIES IN it_bank_st
     where BUKRS = it_bank_st-BUKRS
      and  GJAHR = it_bank_st-GJAHR
      and MONAT = it_bank_st-MONAT
      and ZDIMCID = it_bank_st-ZDIMCID
      and ZDIMLINE = it_bank_st-ZDIMLINE
      and ZDIMUSCOD = it_bank_st-ZDIMUSCOD.
    loop at it_zdimwsbk_tmp ASSIGNING <fs_zdimwsbk>.
      read table it_bank_st with key BUKRS = <fs_zdimwsbk>-BUKRS
                                     GJAHR = <fs_zdimwsbk>-GJAHR
                                     MONAT = <fs_zdimwsbk>-MONAT
                                     ZDIMCID = <fs_zdimwsbk>-ZDIMCID
                                     ZDIMLINE = <fs_zdimwsbk>-ZDIMLINE
                                     ZDIMUSCOD = <fs_zdimwsbk>-ZDIMUSCOD.
      if sy-subrc eq 0.
        <fs_zdimwsbk>-ZDIMFLAG = it_bank_st-ZDIMFLAG.
        <fs_zdimwsbk>-ZDIMTYPE = it_bank_st-ZDIMTYPE.
        <fs_zdimwsbk>-GJAHR1 = it_bank_st-GJAHR1.
        <fs_zdimwsbk>-MONAT1 = it_bank_st-MONAT1.
        <fs_zdimwsbk>-BELNR = it_bank_st-BELNR.
        <fs_zdimwsbk>-BUZEI = it_bank_st-BUZEI.
        <fs_zdimwsbk>-ZDIMINDIE = it_bank_st-ZDIMINDIE.
        "changed by d00194 at 090526 for record date and time
        <fs_zdimwsbk>-zdimdate = sy-datum.
        <fs_zdimwsbk>-zdimtime = sy-uzeit.
      endif.
    endloop.
  endif.
  IF g_flg_ind NE 'X'.

    UPDATE zdimbsbs FROM TABLE it_zdimbsbs_tmp.
    IF sy-subrc = 0.
      UPDATE zdimwsbk FROM TABLE it_zdimwsbk_tmp.
      IF sy-subrc = 0.
*      modify zpwseg
        DELETE it_out WHERE zdimflag = 'X' AND edit = 'X'.
        MESSAGE  s888(sabapdocu) WITH '对账数据已保存'.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'DEQUEUE_ALL'.  "解锁
ENDFORM.                    "f2_deal_table
*&---------------------------------------------------------------------*
*&      Form  frm_f_answer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f_answer .
  DATA l_answer TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '确认对话框'
      text_question         = '是否查看余额调节表？'
      text_button_1         = '是'
      text_button_2         = '否'
      default_button        = '1'
      display_cancel_button = space
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    LEAVE TO SCREEN 0.
  ENDIF.

  IF l_answer = '1'.
*   调用余额调节表程序
    SUBMIT zdimension_fii006_A WITH p_bukrs = p_bukrs
            WITH p_hkont = g_hkont
            with p_uscod = p_zuscod
            WITH p_gjahr = p_gjahr
            WITH p_monat = p_monat
            AND RETURN.
  ELSEIF l_answer = '2'.

  ENDIF.
ENDFORM.                    " frm_f_answer
*&---------------------------------------------------------------------*
*&      Form  frm_cancel_belnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_cancel_belnr .
  DATA : s_out TYPE TABLE OF typ_out WITH HEADER LINE,
          h_out TYPE TABLE OF typ_out WITH HEADER LINE,
          modify_out TYPE TABLE OF zdimbsbs WITH HEADER LINE,
          one_more TYPE TABLE OF zdimbsbs WITH HEADER LINE.
  DATA : l_answer TYPE c,
         l_flg TYPE c,
         n_flg TYPE c,
         l_dmbtr TYPE bsis-dmbtr.
  CLEAR : s_out,h_out.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '确认对话框'
      text_question         = '确定要清除冲销的会计凭证吗？'
      text_button_1         = '是'
      text_button_2         = '否'
      default_button        = '1'
      display_cancel_button = space
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    LEAVE TO SCREEN 0.
  ENDIF.
  CLEAR n_flg.
  IF l_answer = '1'.
    LOOP AT it_out.
      IF it_out-edit = 'X'.
        n_flg = 'X'.
      ENDIF.
      IF it_out-shkzg = 'S'.
        APPEND it_out TO s_out.
      ELSEIF it_out-shkzg = 'H'.
        APPEND it_out TO h_out.
      ENDIF.
      CLEAR it_out.
    ENDLOOP.
    SORT s_out BY dmbtr.
    SORT h_out BY dmbtr.
    IF n_flg = ''.
      LOOP AT s_out.
        LOOP AT h_out.
*      WHERE belnr <> s_out-belnr AND dmbtr = s_out-dmbtr.
          IF s_out-dmbtr = h_out-dmbtr.
            s_out-zdimflag = 'X'.
            s_out-zdimtype = '3'.
            s_out-monat1 = p_monat.
            s_out-gjahr1 = p_gjahr.
            h_out-zdimflag = 'X'.
            h_out-zdimtype = '3'.
            h_out-monat1 = p_monat.
            h_out-gjahr1 = p_gjahr.
            MOVE-CORRESPONDING s_out TO modify_out.
            modify_out-zdimcabel = 'X'.
            APPEND modify_out.
            CLEAR modify_out.
            MOVE-CORRESPONDING h_out TO modify_out.
            modify_out-zdimcabel = 'X'.
            APPEND modify_out.
            CLEAR modify_out.
            DELETE h_out.
            DELETE s_out.
            CLEAR : s_out,h_out.
            l_flg = 'X'.
            EXIT.
          ELSE.
            l_flg = ''.
          ENDIF.
        ENDLOOP.
        IF l_flg = 'X'.
          CONTINUE.
        ENDIF.
      ENDLOOP.

*    LOOP AT s_out.
*      LOOP AT h_out WHERE belnr = s_out-belnr AND buzei <> s_out-buzei
*                                              AND dmbtr  = s_out-dmbtr.
*        s_out-oflag = 'X'.
*        s_out-otype = '3'.
*        h_out-oflag = 'X'.
*        h_out-otype = '3'.
*        MOVE-CORRESPONDING s_out TO modify_out.
*        modify_out-cabel = 'X'.
*        APPEND modify_out.
*        CLEAR modify_out.s
*        MOVE-CORRESPONDING h_out TO modify_out.
*        modify_out-cabel = 'X'.
*        APPEND modify_out.
*        CLEAR modify_out.
*        DELETE h_out.
*        DELETE s_out.
*        EXIT.
*      ENDLOOP.
*    ENDLOOP.

      UPDATE zdimbsbs FROM TABLE modify_out.
      IF sy-subrc = 0.
        MESSAGE s888(sabapdocu) WITH '已清除冲销的会计凭证！'.
        COMMIT WORK AND WAIT.

        LOOP AT it_out.
          READ TABLE modify_out WITH KEY belnr = it_out-belnr
                                         buzei = it_out-buzei
                                         dmbtr = it_out-dmbtr.
          IF sy-subrc = 0.
            DELETE it_out.
          ENDIF.
        ENDLOOP.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
    CLEAR: l_dmbtr.
    LOOP AT it_out WHERE edit = 'X' AND zdimflag = ''.
      IF it_out-shkzg = 'S'.
        l_dmbtr = l_dmbtr + it_out-dmbtr.
      ELSEIF it_out-shkzg = 'H'.
        l_dmbtr = l_dmbtr - it_out-dmbtr.
      ENDIF.
    ENDLOOP.

    IF l_dmbtr = 0.
      CLEAR : it_bsis,one_more.
      LOOP AT it_out WHERE edit = 'X' AND zdimflag = ''.
        READ TABLE it_bsis WITH KEY belnr = it_out-belnr
                                   buzei = it_out-buzei
                                   dmbtr = it_out-dmbtr
                                   budat = it_out-budat
                                   bldat = it_out-bldat.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING it_out TO one_more.
          one_more-zdimcabel = 'X'.
          one_more-zdimflag = 'X'.
          one_more-zdimtype = '3'.
          one_more-monat1 = p_monat.
          one_more-gjahr1 = p_gjahr.
          APPEND one_more.
          CLEAR one_more.
        ENDIF.
      ENDLOOP.

      UPDATE zdimbsbs FROM TABLE one_more.
      IF sy-subrc = 0.
        IF NOT one_more IS INITIAL.
          MESSAGE s888(sabapdocu) WITH '已清除勾选的会计凭证项目！'.
        ENDIF.
        COMMIT WORK AND WAIT.
        DELETE it_out WHERE edit = 'X' AND zdimflag = ''.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      MESSAGE e888(sabapdocu) WITH
      '已勾选要清除项目凭证的金额不匹配，请查证！'.
      STOP.
    ENDIF.
*
*    IF l_dmbtr = 0 AND l_oflag = ''.
*      LOOP AT i_out WHERE edit = 'X'.
*        UPDATE zbsbs SET oflag = 'X'
*                     otype = '3'
*                     cabel = 'X'
*               WHERE bukrs = p_bukrs
*               AND   hkont = p_hkont
*               AND   gjahr = p_gjahr
*               AND   belnr = i_out-belnr
*               AND   buzei = i_out-buzei.
*        IF sy-subrc = 0.
*          DELETE i_out.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*      ENDLOOP.
*      MESSAGE s888(sabapdocu) WITH '已清除勾选的会计凭证项目！'.
*    ELSE.
*      IF v_oflag = 'X'.
*        MESSAGE w888(sabapdocu) WITH
*         '已勾选要清除项目凭证的金额不正确，请查证！'.
*      ENDIF.
*    ENDIF.
  ELSEIF l_answer = '2'.
    MESSAGE s888(sabapdocu) WITH '操作已取消！'.
  ENDIF.
ENDFORM.                    " frm_cancel_belnr
*&---------------------------------------------------------------------*
*&      Form  frm_look_belnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_look_belnr .
  CLEAR it_out.
  READ TABLE it_out INDEX v_slis_field-tabindex.
  SET PARAMETER ID 'BUK' FIELD p_bukrs.
  SET PARAMETER ID 'BLN' FIELD it_out-belnr.
  SET PARAMETER ID 'GJR' FIELD p_gjahr.
  IF it_out-belnr = ''.
    MESSAGE s888(sabapdocu) WITH '此项目凭证为空！'.
  ELSE.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    " frm_look_belnr
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'MAIN9000'.
  g_repid = sy-repid.
  IF custom_container1 IS INITIAL.
*
    PERFORM frm_select_docu CHANGING it_screen_out.

* create a custom container control for our ALV Control
    CREATE OBJECT custom_container1
        EXPORTING
            container_name = cont_on_main
        EXCEPTIONS
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
* add your handling, for example
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = g_repid
          txt2  = sy-subrc
          txt1  = 'The control could not be created'(510).
    ENDIF.

* create an instance of alv control
    CREATE OBJECT grid1
           EXPORTING i_parent = custom_container1.
* Set a titlebar for the grid control
    gs_layout1-grid_title = '对帐备选记录'(100).
* allow to select multiple lines
    gs_layout1-sel_mode = 'A'.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_variant           = alv_variant
        i_save               = 'A'
        is_layout            = gs_layout1
        it_toolbar_excluding = alv_exclude
      CHANGING
        it_outtab            = it_screen_out
        it_fieldcatalog      = tt_fieldcat.

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_user_command FOR grid1.
    SET HANDLER event_receiver->handle_toolbar FOR grid1.

* ?4.Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD grid1->set_toolbar_interactive.
  ELSE.
    CALL METHOD grid1->refresh_table_display.
  ENDIF.                               "IF grid1 IS INITIAL
  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = grid1.


ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  frm_select_docu
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_SCREEN_OUT  text
*----------------------------------------------------------------------*
FORM frm_select_docu  CHANGING i_screen_out LIKE it_screen_out[].
  DATA wa_fieldcat LIKE LINE OF tt_fieldcat.
  DATA l_pos TYPE i.
  DEFINE m_fieldcat.
    clear wa_fieldcat.
    wa_fieldcat-col_pos = l_pos + 1.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-seltext = &2.
    wa_fieldcat-coltext = &2.
    wa_fieldcat-outputlen = &3.
    append wa_fieldcat to tt_fieldcat.
  END-OF-DEFINITION.
  IF r1 = 'X'.  "根据会计凭证对帐
*    m_fieldcat   'BUKRS' '公司代码'   '8'.
*    m_fieldcat   'USCOD' '银行账号'   '20'.
*    m_fieldcat   'GJAHR' '会计年度'   '8'.
*    m_fieldcat   'MONAT' '期间'       '4'.
    m_fieldcat   'ZDIMCID' '银行流水号' '10'.
    m_fieldcat   'ZDIMLINE' '行项目'    '6'.
    m_fieldcat   'ZDIMNAME' '对方用户名' '24'.
    m_fieldcat   'ZDIMCSCOD' '对方银行帐号' '20'.
    m_fieldcat   'ZDIMEDATE' '交易日期'   '10'.
    m_fieldcat  'SHKZG1' '借贷标识'   '6'.
    m_fieldcat  'DMBTR1' '金额'       '13'.
    m_fieldcat  'ZDIMDTEXT'  '摘要'      '40'.
  ELSE.        "根据银行帐单对帐
*    m_fieldcat  'BUKRS'  '公司代码'  '8'.
*    m_fieldcat  'HKONT'  '会计科目'  '10'.
*    m_fieldcat  'GJAHR'  '会计年度'  '8'.
    m_fieldcat  'BELNR'  '会计凭证'  '10'.
    m_fieldcat  'BUZEI'  '行项目'  '6'.
    m_fieldcat  'BUDAT'  '记帐日期'  '10'.
    m_fieldcat  'BLDAT'  '凭证日期'  '10'.
    m_fieldcat  'MONAT'  '期间'  '4'.
    m_fieldcat  'SHKZG'  '借贷标识'  '4'.
    m_fieldcat  'DMBTR'  '金额'  '13'.
*    m_fieldcat  'WAERS'  '货币'  '4'.
    m_fieldcat  'XREF3'  '对方帐号'  '20'.
    m_fieldcat  'SGTXT' '摘要' '50'.
  ENDIF.
  REFRESH i_screen_out.
  IF r1 = 'X'.
    LOOP AT it_bank.
      CLEAR wa_out.
      wa_out-bukrs = it_bank-bukrs.
      wa_out-gjahr1 = it_bank-gjahr.
      wa_out-monat1 = it_bank-monat.
      wa_out-zdimuscod = it_bank-zdimuscod.
      wa_out-zdimcid = it_bank-zdimcid.
      wa_out-zdimline = it_bank-zdimline.
      wa_out-zdimname = it_bank-zdimname.
      wa_out-zdimcscod = it_bank-zdimcscod.
      wa_out-zdimedate = it_bank-zdimedate.
      wa_out-shkzg1 = it_bank-shkzg.
      wa_out-dmbtr1 = it_bank-dmbtr.
      wa_out-zdimdtext = it_bank-zdimdtext.
      APPEND wa_out TO it_screen_out.
    ENDLOOP.
  ELSE.
    LOOP AT it_bsis.
      CLEAR wa_out.
      wa_out-bukrs = it_bsis-bukrs.
      wa_out-gjahr = it_bsis-gjahr.
      wa_out-belnr = it_bsis-belnr.
      wa_out-buzei = it_bsis-buzei.
      wa_out-budat = it_bsis-budat.
      wa_out-bldat = it_bsis-bldat.
      wa_out-monat = it_bsis-monat.
      wa_out-shkzg = it_bsis-shkzg.
      wa_out-dmbtr = it_bsis-dmbtr.
      wa_out-xref3 = it_bsis-xref3.
      wa_out-sgtxt = it_bsis-sgtxt.
      wa_out-hkont = it_bsis-hkont.
      APPEND wa_out TO it_screen_out.
    ENDLOOP.
*    wa_out-belnr = '1000'. APPEND wa_out TO i_screen_out.
*    wa_out-belnr = '1200'. APPEND wa_out TO i_screen_out.
  ENDIF.
  LOOP AT it_out2.
    DELETE it_screen_out FROM it_out2.
  ENDLOOP..
ENDFORM.                    " frm_select_docu
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      leave to screen 0.

  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS '9001'.
  SET TITLEBAR '9001'.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  CASE sy-ucomm.
    WHEN 'ENT'.
      PERFORM frm_filter_dmbtr.
      LEAVE TO SCREEN 0.
    WHEN 'CLOSE' OR 'CAN'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Form  frm_filter_dmbtr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_filter_dmbtr .
  IF g_r_sel = 'X'.
    LOOP AT it_out WHERE dmbtr = g_dmbtr AND dmbtr1 = g_dmbtr.
      it_out-edit = 'X'.
      MODIFY it_out.
    ENDLOOP.
  ELSE.
    LOOP AT it_out WHERE dmbtr = g_dmbtr AND dmbtr1 = g_dmbtr.
      it_out-edit = ''.
      MODIFY it_out.
    ENDLOOP.
  ENDIF.
  CLEAR g_dmbtr.
ENDFORM.                    " frm_filter_dmbtr
*&---------------------------------------------------------------------*
*&      Form  frm_f1_compare_2
*&---------------------------------------------------------------------*
*       模糊对帐
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f1_compare_2 .
  DATA l_shkzg TYPE c.
*  SORT i_bsis BY shkzg dmbtr xref3 bldat.
*  SORT i_bank BY shkzg dmbtr cscod ddate.
  LOOP AT it_bsis.
    IF it_bsis-shkzg = 'S'.
      l_shkzg = 'H'.
    ELSEIF it_bsis-shkzg = 'H'.
      l_shkzg = 'S'.
    ENDIF.
    LOOP AT it_bank WHERE shkzg = l_shkzg AND dmbtr = it_bsis-dmbtr.
      PERFORM frm_f1_compare_table.
      DELETE it_bank.
      EXIT.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " frm_f1_compare_2
*&---------------------------------------------------------------------*
*&      Form  frm_f1_compare_3
*&---------------------------------------------------------------------*
*       手工对帐：根据会计凭证找银行对帐单
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f1_compare_3 .
* 在屏幕上显示未对帐的凭证“会计凭证”或银行帐单凭证,然后通过双击操作来
* 进行手工对帐。
  IF r1 = 'X'.  "根据会计凭证对帐
    CLEAR it_bank.
    LOOP AT it_bsis.
      PERFORM frm_f1_compare_table.
    ENDLOOP.
  ELSEIF r2 = 'X'.  "根据银行对帐单对帐
    CLEAR it_bsis.
    LOOP AT it_bank.
      PERFORM frm_f1_compare_table.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " frm_f1_compare_3
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_zdimuscod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ZUSCOD  text
*----------------------------------------------------------------------*
FORM FRM_GET_zdimuscod  USING   P_ZUSCOD like ZDiMBUHK-zdimuscod.
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
  "changed by d00194 at 090525
  select * into CORRESPONDING FIELDS OF TABLE it_zuscod
    from ZDiMBUHK
    where BUKRS = i_bukrs
    AND ZDIMFLAG2 = 'X'
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
      dynpprog        = l_repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_ZUSCOD'  "屏幕上的名称
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
    read table RETURN_TAB index 1.
    P_ZUSCOD = RETURN_TAB-fieldval.
  ENDIF.
ENDFORM.                    " FRM_GET_zdimuscod
