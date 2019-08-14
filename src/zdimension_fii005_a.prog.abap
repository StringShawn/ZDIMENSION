*&---------------------------------------------------------------------*
*& Report  ZDIMENSION_FII005_A
* Purpose               :                                              *
* Project Name          :  达美银行对账项目                            *
* Created by            :  D00194                           *
* Create on             :  2009-05-11

*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDIMENSION_FII005_A NO STANDARD PAGE HEADING MESSAGE-ID 00.

TABLES : zdimwkpf,zdimwsbk,zdimblock,zdimwseg,zdimbsbs,bsis,bsas,bkpf.
TABLES : zdimscnro.
TABLES t001.

DATA wherecode TYPE string.
*&----------主输出类型-------------*
DATA : BEGIN OF output,
        selyn,
        order(4)  TYPE c,               "行号
                                        "ID
        bukrs     LIKE  bsas-bukrs,     "公司代码
        gjahr     LIKE  bsas-gjahr,
        monat     LIKE  bsas-monat,
        budat     LIKE  bsas-budat,     "记帐日期
        bldat     LIKE  bsas-bldat,     "凭证日期
        belnr     LIKE  bsas-belnr,     "会计凭证号
        buzei     LIKE  bsas-buzei,     "会计凭证行项目号
        hkont     LIKE  bsas-hkont,     "会计科目号
        zdimuscod TYPE  zdimuscod,      "银行账号""
        txt50     LIKE  skat-txt50,     "会计科目描述
        waers     LIKE  bsas-waers,     "货币
        qshkzg    LIKE  bsas-shkzg,     "借贷标志
        qdmbtr    LIKE  bsas-dmbtr,     "凭证金额
        xref3     LIKE  bsas-xref3,     "参考码3(银行帐号后20位)
        sgtxt     LIKE  bsas-sgtxt,     "摘要-企业方面
                                            "ID
        zdimcid   LIKE  zdimwsbk-zdimcid,   "流水帐号
        ZDIMLINE  LIKE  zdimwsbk-ZDIMLINE,  "流水帐行项目号
        monat1    LIKE  zdimwsbk-monat,   "银行流水帐期间
        gjahr1    LIKE  zdimwsbk-gjahr,   "银行流水账年度
        zdimname  LIKE  zdimwsbk-zdimname,   "对方户名
        zdimcscod LIKE  zdimwsbk-zdimcscod,   "对方帐号
        zdimedate LIKE  zdimwsbk-zdimedate,   "交易日期 对应bsas-bldat
        yshkzg    LIKE  zdimwsbk-shkzg,   "借贷标志
        ydmbtr    LIKE  zdimwsbk-dmbtr,   "金额
        zdimdtext LIKE  zdimwsbk-zdimdtext,   "摘要-银行方面
        zdimtype  LIKE  zdimwsbk-zdimtype, "对账类型
        zdimflag  LIKE  zdimwsbk-zdimflag,   "对帐完成标志
       END OF output.


DATA : dt_output LIKE STANDARD TABLE OF output WITH HEADER LINE, "输出内表
       ds_output LIKE output,
       tmp_itab  LIKE TABLE OF output,
       dt_bsas   LIKE STANDARD TABLE OF zdimbsbs WITH HEADER LINE,
       dt_bank   LIKE STANDARD TABLE OF zdimwsbk WITH HEADER LINE,
       lt_bank LIKE STANDARD TABLE OF zdimwsbk,
       lt_bsas LIKE STANDARD TABLE OF zdimbsbs,
       dt_lock   LIKE TABLE OF zdimblock,
       d_dmbtr   TYPE p DECIMALS 2,
       ds_lock   LIKE zdimblock,
       l_bankn   TYPE zdimbuhk-zdimuscod,
       l_monat   TYPE zdimblock-monat,
       l_txt50   TYPE skat-txt50,
       r_sel     TYPE c,
       r_unsel   TYPE c,
       flg_ind   TYPE c,
       flg_zbsbs TYPE c,
       flg_zwsbk TYPE c.


*&--------------ALV表输出格式设定----------------------*
*TYPE-POOLS rsds.
TYPE-POOLS slis.                                 "类型组
DATA : g_fieldcat_alv TYPE slis_t_fieldcat_alv,  "主体表列格式
       gd_fieldcat_alv TYPE slis_t_fieldcat_alv, "追溯表列格式
       g_fieldcat TYPE slis_fieldcat_alv,        "列格式结构
       g_layout TYPE slis_layout_alv,            "函数填充列格式
       g_repid LIKE sy-repid,                    "显示函数当前程序
       g_callback_pf_status TYPE
       slis_formname VALUE 'PF_STATUS',          "GUI设定
       g_callback_usercommand TYPE slis_formname VALUE 'USER_COMM'.
DATA:  control_title TYPE lvc_title.
DATA   git_events TYPE slis_t_event.


DATA p_hkont LIKE skat-saknr.
DATA zdimflag2 TYPE zdimflag2.


*&------------------状态消息宏-------------------------*
DEFINE write_state.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 0
      text       = &1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*   INCLUDE ZFIR05_CUSTOM_SCREEN                                       *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fra0 WITH FRAME TITLE text-000.
SELECTION-SCREEN BEGIN OF BLOCK fra1 WITH FRAME TITLE text-001.
PARAMETERS : p_bukrs LIKE t001-bukrs OBLIGATORY, "公司代码
             "p_hkont LIKE skat-saknr OBLIGATORY,               "会计科目

             p_uscod TYPE zdimuscod OBLIGATORY,                "银行账号

             p_gjahr LIKE zdimwseg-gjahr OBLIGATORY DEFAULT sy-datum+0(4),
                                                               "会计年度
             p_monat LIKE bsis-monat OBLIGATORY DEFAULT sy-datum+4(2).
"会计期间
SELECT-OPTIONS : s_otype FOR zdimwsbk-zdimtype NO-EXTENSION.        "对帐类型

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK fra3 WITH FRAME TITLE text-010.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_yiqing RADIOBUTTON GROUP rad DEFAULT 'X'.  "已清帐单
SELECTION-SCREEN COMMENT 3(12) text-011 FOR FIELD p_yiqing.

PARAMETERS : p_qywq RADIOBUTTON GROUP rad.                "企业未清帐单
SELECTION-SCREEN COMMENT 18(12) text-006 FOR FIELD p_qywq.
PARAMETERS : p_yhwq RADIOBUTTON GROUP rad.                "银行未清帐单
SELECTION-SCREEN COMMENT 33(12) text-012 FOR FIELD p_yhwq.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK fra3.

SELECTION-SCREEN END OF BLOCK fra1.

SELECTION-SCREEN BEGIN OF BLOCK fra2 WITH FRAME TITLE text-016.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_look RADIOBUTTON GROUP rad2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(12) text-008 FOR FIELD p_look.      "查询对帐
"PARAMETERS : p_cabel RADIOBUTTON GROUP rad2.                    "冲销凭证
"SELECTION-SCREEN COMMENT 18(12) text-015 FOR FIELD p_cabel.
PARAMETERS : p_lock RADIOBUTTON GROUP rad2.                    "锁定对帐
SELECTION-SCREEN COMMENT 18(12) text-013 FOR FIELD p_lock.
PARAMETERS : p_unlock RADIOBUTTON GROUP rad2.                  "取消锁定
SELECTION-SCREEN COMMENT 33(12) text-014 FOR FIELD p_unlock.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK fra2.
SELECTION-SCREEN END OF BLOCK fra0.

*&------------------------------------------------------------------------*
************************************************************************
*                      INITIALIZATION                                  *
************************************************************************
INITIALIZATION.
*获取当前使用版本为附加字段OR 非附加字段
data: g_fieldname like dd03l-fieldname,
      result type ZDIMSCNRO.

CLEAR result.
 CALL FUNCTION 'ZDIM_SCENARIO_CHECK'
  IMPORTING
    RETURN            = result
  EXCEPTIONS
    NO_DATA           = 1
    ERROR_FIELD       = 2
    OTHERS            = 3
           .
 IF SY-SUBRC = 1.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   message e888(sabapdocu) WITH text-024.
 ELSEIF SY-SUBRC = 2.
  message e888(sabapdocu) WITH text-025.
 ELSEIF SY-SUBRC = 0.
  g_fieldname = result-fieldname.
 ENDIF.

*end of 获取当前使用版本为附加字段OR 非附加字段



*-------------------------------------------------------------------------------
at SELECTION-SCREEN on VALUE-REQUEST FOR p_USCOD.
  perform FRM_GET_zdimuscod using p_USCOD.
*&------------------------------------------------------------------------*
START-OF-SELECTION.
DATA: BEGIN OF i_bukrs OCCURS 0,
           bukrs LIKE t001k-bukrs,
           werks LIKE t001k-bwkey,"评估范围
        END OF i_bukrs.
  DATA:g_str TYPE string.

SELECT bukrs
*         bwkey AS werks
  INTO TABLE i_bukrs
  FROM t001
  WHERE bukrs = p_bukrs.

  IF LINES( i_bukrs ) = 0.
    MESSAGE '请重新选择条件，不存在公司条件' TYPE 'I'.
    STOP.
  ELSE.
    clear zdimflag2.
    SELECT SINGLE zdimflag2 FROM zdimbuhk
      INTO zdimflag2
      WHERE bukrs = p_bukrs
      AND zdimuscod = p_uscod.
      IF sy-subrc ne 0 or zdimflag2 ne 'X'.
        MESSAGE i888(sabapdocu) WITH text-111.
        stop.
      ENDIF.

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

*根据银行账号获取总账科目--------------------------------------------------
IF p_uscod IS NOT INITIAL.
  SELECT SINGLE HKONT FROM ZDIMBUHK
    INTO p_hkont
    WHERE ZDIMUSCOD = p_uscod AND bukrs = p_bukrs AND zdimflag2 = 'X'.
    IF sy-subrc ne 0.
      MESSAGE i888(sabapdocu) WITH text-111.
      STOP.
    ENDIF.
ENDIF.
*根据选择，执行功能--------------------------------------------------------

  IF p_look = 'X'."查询或取消对帐
    PERFORM get_data.     "get data from database and move data
    "into dt_output if data accord with query
    "condition.
    PERFORM alv_layout.
    PERFORM alv_field.
    PERFORM display_report.
*
  ELSEIF p_lock = 'X' OR p_unlock = 'X'."锁定或取消锁定对帐
    PERFORM lock_data.
*for cabel
*  ELSEIF p_cabel = 'X'.
*    PERFORM get_cabel.
*    PERFORM alv_layout.
*    PERFORM alv_field.
*    PERFORM display_report.
  ENDIF.

  AT SELECTION-SCREEN.
  IF p_monat < 1 OR p_monat > 12.
    MESSAGE e888(sabapdocu) WITH '请输入正确的会计期间！'.
  ENDIF.
  PERFORM pur_check."权限检查

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
write_state '正在获取相关数据，请稍候...'.
*-----------------------取数据--------------------------*
"removed for hkont
  CLEAR l_bankn.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_hkont
    IMPORTING
      output = p_hkont.
*获取与会计科目对应的银行帐号
*  SELECT SINGLE zdimuscod INTO l_bankn FROM zdimbuhk
*    WHERE bukrs = p_bukrs
*    AND   hkont = p_hkont.
  l_bankn = p_uscod.

  IF p_yiqing = 'X'.
    PERFORM get_yiqing_bill."取已清帐单
  ELSEIF p_qywq = 'X'.
    PERFORM get_enterprise_weiqing."取企业未清帐单
  ELSE.
    PERFORM get_bank_weiqing."取银行未清帐单
  ENDIF.
  lt_bank = dt_bank[].
  lt_bsas = dt_bsas[].
  PERFORM consummate_output_data.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_yiqing_bill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_yiqing_bill .
*企业已清账单
  PERFORM get_enterprise_yiqing.

*银行已清账单
  PERFORM get_bank_yiqing.

*将数据移到输出内表
* 手工对账：根据银行对账单对长：BSAS：zdimWSEG = N：1
* 循环会计表取数据:参考字段，银行对账流水号  SORT dt_bank BY belnr buzei
  SORT dt_bank BY zdimcid zdimline.
  LOOP AT dt_bsas WHERE zdimtype = '3' AND zdimindie = 'X'.
    CLEAR dt_bank.
    READ TABLE dt_bank WITH KEY  zdimcid = dt_bsas-zdimcid
                                zdimline = dt_bsas-zdimline
                                BINARY SEARCH.

    IF sy-subrc = 0.
      PERFORM move_data.
    ELSE.

    ENDIF.
  ENDLOOP.
* 手工对账：根据会计凭证对账BSAS:zdimWSEG = 1:N，参考字段：银行账单
* 循环银行凭证号，参考字段：会计凭证
  SORT dt_bsas BY belnr buzei.
  LOOP AT dt_bank WHERE zdimtype = '3' AND zdimindie = 'X'.
    CLEAR dt_bsas.
    READ TABLE dt_bsas WITH KEY belnr = dt_bank-belnr
                                buzei = dt_bank-buzei
                                BINARY SEARCH.
    IF sy-subrc = 0.
      PERFORM move_data.
    ENDIF.
  ENDLOOP.
* 精确对账 模糊对账
  SORT dt_bank BY zdimcid zdimline.
  LOOP AT dt_bsas WHERE zdimtype NE '3'.
    CLEAR dt_bank.
    READ TABLE dt_bank WITH KEY zdimcid = dt_bsas-zdimcid
                                zdimline = dt_bsas-zdimline
                                BINARY SEARCH.
    IF sy-subrc = 0.
      PERFORM move_data.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_yiqing_bill
*&---------------------------------------------------------------------*
*&      Form  get_enterprise_yiqing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_enterprise_yiqing .
SELECT * FROM zdimbsbs INTO CORRESPONDING FIELDS OF TABLE dt_bsas
           WHERE bukrs = p_bukrs
           "AND   hkont = p_hkont
           AND   gjahr1 = p_gjahr
           AND   monat1 = p_monat
           AND   zdimtype IN s_otype
           AND   zdimuscod = p_uscod""
           AND   zdimflag = 'X'.

ENDFORM.                    " get_enterprise_yiqing
*&---------------------------------------------------------------------*
*&      Form  get_bank_yiqing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bank_yiqing .
SELECT * FROM zdimwsbk INTO CORRESPONDING FIELDS OF TABLE dt_bank
           WHERE bukrs  = p_bukrs
           AND   gjahr1  = p_gjahr
           AND   monat1  = p_monat
           "AND   hkont  = p_hkont
           AND   zdimuscod = p_uscod""
           AND   zdimtype  IN s_otype
           AND   zdimflag  = 'X'.

ENDFORM.                    " get_bank_yiqing
*&---------------------------------------------------------------------*
*&      Form  consummate_output_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consummate_output_data .
SORT dt_output BY belnr buzei zdimcid zdimline.
  LOOP AT dt_output.
    "removed for hkont
    SELECT SINGLE txt50 INTO dt_output-txt50 FROM skat
        WHERE saknr = p_hkont.

    CLEAR wherecode.
    CONCATENATE 'bukrs = dt_output-bukrs AND gjahr = dt_output-gjahr  AND   hkont = dt_output-hkont AND   belnr = dt_output-belnr AND   buzei = dt_output-buzei AND'
    g_fieldname '= dt_output-zdimuscod' INTO wherecode SEPARATED BY space.


      DATA: BEGIN OF bseg_int OCCURS 0.
        INCLUDE STRUCTURE bseg.
      DATA: END OF bseg_int.

      SELECT * FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE bseg_int
      WHERE (wherecode)
      .

      SELECT SINGLE budat waers FROM bkpf
        INTO CORRESPONDING FIELDS OF dt_output
        WHERE bukrs = bseg_int-bukrs
        AND gjahr = bseg_int-gjahr
        AND BELNR = bseg_int-BELNR.





*by d00194 BSAS BSIS不支持附加字段
*    SELECT SINGLE budat waers FROM bsas INTO CORRESPONDING
*      FIELDS OF dt_output
*        WHERE bukrs = dt_output-bukrs
*        AND   gjahr = dt_output-gjahr
*        AND   monat = dt_output-monat
*        AND   hkont = dt_output-hkont
*        AND   belnr = dt_output-belnr
*        AND   buzei = dt_output-buzei.
*    IF sy-subrc <> 0.
*      SELECT SINGLE budat waers FROM bsis INTO CORRESPONDING
*        FIELDS OF dt_output
*          WHERE bukrs = dt_output-bukrs
*          AND   gjahr = dt_output-gjahr
*          AND   monat = dt_output-monat
*          AND   hkont = dt_output-hkont
*          AND   belnr = dt_output-belnr
*          AND   buzei = dt_output-buzei.
*    ENDIF.
    dt_output-order = sy-tabix.
    MODIFY dt_output.
  ENDLOOP.

ENDFORM.                    " consummate_output_data
*&---------------------------------------------------------------------*
*&      Form  get_enterprise_weiqing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_enterprise_weiqing .
SELECT * FROM zdimbsbs INTO CORRESPONDING FIELDS OF TABLE dt_bsas
           WHERE bukrs = p_bukrs
           "AND   hkont = p_hkont
           AND   gjahr = p_gjahr
           AND   monat = p_monat
           AND   zdimuscod = p_uscod""
           AND   zdimflag = ''.
*企业未清帐单
  IF dt_bank IS INITIAL.
    CLEAR dt_bsas.
    LOOP AT dt_bsas.
      PERFORM move_data.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_enterprise_weiqing
*&---------------------------------------------------------------------*
*&      Form  get_bank_weiqing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bank_weiqing .
SELECT * FROM zdimwsbk INTO CORRESPONDING FIELDS OF TABLE dt_bank
           WHERE bukrs  = p_bukrs
           AND   gjahr  = p_gjahr
           AND   monat  = p_monat
           "AND   hkont  = p_hkont
           AND   zdimuscod = p_uscod""
           AND   zdimflag  = ''.
*银行未清帐单
  IF dt_bsas IS INITIAL.
    CLEAR dt_bank.
    LOOP AT dt_bank.
      MOVE-CORRESPONDING dt_bank TO dt_output.
      dt_output-yshkzg = dt_bank-shkzg.
      dt_output-ydmbtr = dt_bank-dmbtr.
      dt_output-budat  = dt_bank-zdimedate.
      APPEND dt_output.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_bank_weiqing
*&---------------------------------------------------------------------*
*&      Form  move_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_data .
MOVE-CORRESPONDING dt_bsas TO dt_output.
  dt_output-qshkzg = dt_bsas-shkzg.
  dt_output-qdmbtr = dt_bsas-dmbtr.

  dt_output-zdimcid = dt_bank-zdimcid.
  dt_output-zdimline = dt_bank-zdimline.
  dt_output-gjahr1 = dt_bank-gjahr.
  dt_output-monat1 = dt_bank-monat.
  dt_output-xref3  = dt_bank-zdimcscod.
  dt_output-zdimname  = dt_bank-zdimname.
  dt_output-zdimcscod  = dt_bank-zdimcscod.
  dt_output-zdimedate  = dt_bank-zdimedate.
  dt_output-yshkzg = dt_bank-shkzg.
  dt_output-ydmbtr = dt_bank-dmbtr.
  dt_output-zdimdtext  = dt_bank-zdimdtext.
  APPEND dt_output.

ENDFORM.                    " move_data
*&---------------------------------------------------------------------*
*&      Form  alv_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_layout .
  g_layout-zebra = 'X'.
  g_layout-detail_popup = 'X'.
  g_layout-info_fieldname = 'COLOR'.        "行颜色设定
  g_repid = sy-repid.                       "定义当前程序
  g_layout-no_vline = ' '.                  "显示竖线
  g_layout-colwidth_optimize = 'X'.         "优化列宽
  g_layout-detail_initial_lines = 'X'.
  g_layout-detail_titlebar = '详细内容'.    "明细记录对话框标题.

ENDFORM.                    " alv_layout
*&---------------------------------------------------------------------*
*&      Form  alv_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_field .
DATA : pos TYPE i.
  pos = 1.
  REFRESH g_fieldcat_alv.

  IF p_yiqing = 'X'
    ""OR p_cabel = 'X' "for cabel
    .
    g_fieldcat-col_pos = pos.
    g_fieldcat-fieldname = 'SELYN'.
    g_fieldcat-seltext_l = '选择'.
    g_fieldcat-checkbox  = 'X'.
    g_fieldcat-edit      = 'X'.
*  g_fieldcat-hotspot = 'X' .
*  g_fieldcat-emphasize = 'X'.
    APPEND g_fieldcat TO g_fieldcat_alv.
    CLEAR g_fieldcat.
    pos = pos + 1.
  ENDIF.

  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ORDER'.
  g_fieldcat-seltext_l = '行号'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'BUKRS'.
  g_fieldcat-seltext_l = '公司代码'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'BUDAT'.
  g_fieldcat-seltext_l = '记帐日期'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'BLDAT'.
  g_fieldcat-seltext_l = '凭证日期'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'BELNR'.
  g_fieldcat-seltext_l = '会计凭证号'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'BUZEI'.
  g_fieldcat-seltext_l = '凭证行项目号'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'HKONT'.
  g_fieldcat-seltext_l = '会计科目'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'TXT50'.
  g_fieldcat-seltext_l = '会计科目描述'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

   pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMUSCOD'.
  g_fieldcat-seltext_l = '银行帐号 '.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'WAERS'.
  g_fieldcat-seltext_l = '货币'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'QSHKZG'.
  g_fieldcat-seltext_l = '借贷标志'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'QDMBTR'.
  g_fieldcat-seltext_l = '凭证金额'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'XREF3'.
  g_fieldcat-seltext_l = '参考码3'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'SGTXT'.
  g_fieldcat-seltext_l = '摘要'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMCID'.
  g_fieldcat-seltext_l = '流水号'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMLINE'.
  g_fieldcat-seltext_l = '流水行项目号'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMNAME'.
  g_fieldcat-seltext_l = '对方户名'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMCSCOD'.
  g_fieldcat-seltext_l = '对方帐号'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMEDATE'.
  g_fieldcat-seltext_l = '交易日期'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'YSHKZG'.
  g_fieldcat-seltext_l = '借贷标志'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'YDMBTR'.
  g_fieldcat-seltext_l = '金额'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMDTEXT'.
  g_fieldcat-seltext_l = '摘要'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

  pos = pos + 1.
  g_fieldcat-col_pos = pos.
  g_fieldcat-fieldname = 'ZDIMFLAG'.
  g_fieldcat-seltext_l = '对帐完成标志'.
  APPEND g_fieldcat TO g_fieldcat_alv.
  CLEAR g_fieldcat.

ENDFORM.                    " alv_field
*&---------------------------------------------------------------------*
*&      Form  display_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_report .
g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_pf_status_set = g_callback_pf_status
      i_callback_user_command  = g_callback_usercommand
      i_callback_html_top_of_page = 'TOP_OF_PAGE'
      i_grid_title                = control_title
      i_background_id          = 'ALV_BACKGROUND'
      i_buffer_active          = 'X'
      i_callback_program       = g_repid
*      i_grid_title             = title
      it_fieldcat              = g_fieldcat_alv[]
      is_layout                = g_layout
      i_save                   = 'A'
      it_events                   = git_events[]
    TABLES
      t_outtab                 = dt_output.

ENDFORM.                    " display_report
*&---------------------------------------------------------------------*
*&      Form  lock_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lock_data .
DATA : zwsbk TYPE TABLE OF zdimwsbk.
  DATA : answer(1) TYPE c.
  DATA : ydate TYPE bsas-budat.
  IF p_monat = '12'.
    l_monat = '01'.
  ELSE.
    l_monat = p_monat + 1.
  ENDIF.
  CONCATENATE p_gjahr p_monat INTO ydate.
  IF p_lock = 'X'.
    IF ydate > sy-datum+0(6).
      MESSAGE w888(sabapdocu) WITH '不允许锁定未来的对帐期间！'.
    ELSE.
      CLEAR answer.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          textline1 = '确定要锁定当前期间对帐吗？'
          titel     = '操作确认框'
        IMPORTING
          answer    = answer.
      IF answer = 'J'.
        SELECT * FROM zdimblock INTO CORRESPONDING FIELDS OF TABLE
          dt_lock WHERE bukrs = p_bukrs
                  AND   gjahr = p_gjahr
                  AND   monat = p_monat
                  AND   zdimuscod = p_uscod""
                  "AND   hkont = p_hkont
          .
        IF sy-subrc = 0.
          UPDATE zdimblock SET zdimlock = 'X' WHERE bukrs = p_bukrs
                                        AND   gjahr = p_gjahr
                                        AND   monat = p_monat
                                        AND   zdimuscod = p_uscod""
                                        "AND   hkont = p_hkont
                                        AND   zdimuscod = p_uscod
                                        .
          MESSAGE s888(sabapdocu) WITH '当前对帐期间已被锁定！'.
        ELSE.
          ds_lock-mandt = sy-mandt.
          ds_lock-bukrs = p_bukrs.
          ds_lock-gjahr = p_gjahr.
          ds_lock-monat = p_monat.
          ds_lock-hkont = p_hkont.
          ds_lock-zdimuscod = p_uscod.
          ds_lock-zdimlock = 'X'.
          INSERT into zdimblock values ds_lock.
          MESSAGE s888(sabapdocu) WITH '当前对帐期间已被锁定！'.
        ENDIF.
      ELSE.
        MESSAGE s888(sabapdocu) WITH '锁定操作已取消！'.
      ENDIF.
    ENDIF.
  ELSEIF p_unlock = 'X'.
    SELECT * FROM zdimwsbk INTO CORRESPONDING FIELDS OF TABLE zwsbk
                  WHERE bukrs = p_bukrs
                  "AND   hkont = p_hkont
                  AND   gjahr = p_gjahr
                  AND   monat = l_monat
                  AND   zdimuscod = p_uscod""
                  AND   zdimflag = 'X'
                  AND   zdimtype <> ''.
    IF sy-subrc <> 0.
      UPDATE zdimblock SET zdimlock = '' WHERE bukrs = p_bukrs
                                   AND   gjahr = p_gjahr
                                   AND   monat = p_monat
                                   "AND   hkont = p_hkont
                                   AND   zdimuscod = p_uscod""
                                   .
      IF sy-subrc = 0.
        MESSAGE s888(sabapdocu) WITH '当前对帐期间已被解除锁定！'.
      ELSE.
        MESSAGE s888(sabapdocu) WITH '当前对帐期间没有被锁定，不必解锁！'.
      ENDIF.
    ELSE.
      MESSAGE i888(sabapdocu) WITH
     '当前期间后的期间已发生对帐业务，不能解锁！'.
    ENDIF.
  ENDIF.

ENDFORM.                    " lock_data
*&---------------------------------------------------------------------*
*&      Form  get_cabel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cabel .
SELECT * FROM zdimbsbs INTO CORRESPONDING FIELDS OF TABLE dt_bsas
           WHERE bukrs = p_bukrs
           "AND   hkont = p_hkont
           AND   gjahr1 = p_gjahr
           AND   monat1 = p_monat
           AND   zdimuscod = p_uscod""
           AND   zdimflag = 'X'
           AND   zdimtype = '3'
           AND   zdimcabel = 'X'.

*  IF dt_bank IS INITIAL.
  CLEAR dt_bsas.
  LOOP AT dt_bsas.
    PERFORM move_data.
  ENDLOOP.
  PERFORM consummate_output_data.
*  ENDIF.

ENDFORM.                    " get_cabel
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  "SET TITLEBAR 'TITLE_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
CASE sy-ucomm.
    WHEN 'ENT'.
      PERFORM filter_dmbtr.
      LEAVE TO SCREEN 0.
    WHEN 'CLOSE' OR 'CAN'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  filter_dmbtr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filter_dmbtr .
IF r_sel = 'X'.
    LOOP AT dt_output WHERE ydmbtr = d_dmbtr AND qdmbtr = d_dmbtr.
      dt_output-selyn = 'X'.
      MODIFY dt_output.
    ENDLOOP.
  ELSE.
    LOOP AT dt_output WHERE ydmbtr = d_dmbtr AND qdmbtr = d_dmbtr.
      dt_output-selyn = ''.
      MODIFY dt_output.
    ENDLOOP.
  ENDIF.
  CLEAR d_dmbtr.

ENDFORM.                    " filter_dmbtr
*&---------------------------------------------------------------------*
*&      Form  pur_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pur_check .
DATA : msg TYPE string.
*z_fi_bank
  AUTHORITY-CHECK OBJECT 'Z_FI_BANK' ID 'BUKRS' FIELD p_bukrs
                                     ID 'SAKNR' FIELD p_hkont.
  IF sy-subrc <> 0.
    CONCATENATE '你没有公司代码' p_bukrs
                '及其对应科目' p_hkont '的权限！' INTO msg.
    "MESSAGE e888(sabapdocu) WITH msg.
    "STOP.

  ENDIF.

ENDFORM.                    " pur_check
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PF_STATUS USING rt_extab TYPE slis_t_extab.
*for cabel
*IF p_cabel = 'X'.
*    SET PF-STATUS 'CABEL'.
*  ELSE.
*    SET PF-STATUS 'YIQING'.
*  ENDIF.

  SET PF-STATUS 'YIQING'.

ENDFORM.                    " PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  user_comm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_comm USING f_ucomm    LIKE sy-ucomm
                     f_selfield TYPE slis_selfield.
DATA : g_grid TYPE REF TO cl_gui_alv_grid.
  DATA : answer(1) TYPE c.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_grid.
  CALL METHOD g_grid->check_changed_data.
  f_selfield-refresh = 'X'.

  CASE f_ucomm.
    WHEN 'CANCEL'.
      LOOP AT dt_output INTO ds_output WHERE selyn = 'X'.
      ENDLOOP.

      IF sy-subrc = 0.

        PERFORM f_check_cancel.
        CLEAR answer.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            textline1 = '确定要取消对帐吗？'
            titel     = '操作确认框'
          IMPORTING
            answer    = answer.
        IF answer = 'J'.
          SELECT * FROM zdimblock INTO CORRESPONDING FIELDS OF TABLE
          dt_lock WHERE bukrs = p_bukrs
                  AND   gjahr = p_gjahr
                  AND   monat = p_monat
                  "AND   hkont = p_hkont
                  AND   zdimuscod = p_uscod""
                  AND   zdimlock = 'X'.
          IF sy-subrc = 0.
            MESSAGE e888(sabapdocu) WITH
            '当前期间对帐已被锁定，要取消对帐请先解锁！'.
          ELSE.
            PERFORM deal_data.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE w888(sabapdocu) WITH '请先选择要取消对帐的行项目！'.
      ENDIF.
    WHEN 'ALL'.
      LOOP AT dt_output INTO ds_output.
        ds_output-selyn = 'X'.
        MODIFY dt_output FROM ds_output.
      ENDLOOP.
    WHEN 'SAL'.
      LOOP AT dt_output INTO ds_output.
        ds_output-selyn = ''.
        MODIFY dt_output FROM ds_output.
      ENDLOOP.
    WHEN 'CAB'.
      PERFORM cancel_belnr.
    WHEN 'FILTER'.
      CALL SCREEN 0100 STARTING AT 50 8 ENDING AT 72 12.
      perform filter_dmbtr.
*    WHEN '&F03'.
*     LEAVE TO SCREEN 0.
  ENDCASE.


ENDFORM.                    " user_comm
*&---------------------------------------------------------------------*
*&      Form  cancel_belnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cancel_belnr .
DATA : dg_output LIKE TABLE OF output WITH HEADER LINE.
*       s_out like table of output with header line,
*       h_out like table of output with header line.
  DATA : l_dmbtr LIKE bsis-dmbtr VALUE 0.
  DATA l_answer TYPE c.

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
  IF l_answer = 1.
    LOOP AT dt_bsas WHERE zdimcabel = 'X'.
      READ TABLE dt_output WITH KEY belnr = dt_bsas-belnr
                                    buzei = dt_bsas-buzei
                                    selyn = 'X'.
      IF sy-subrc = 0.
        CLEAR : dt_bsas-zdimflag,dt_bsas-zdimtype,dt_bsas-zdimcabel.
        MODIFY dt_bsas.
      ENDIF.
    ENDLOOP.

    LOOP AT dt_output WHERE zdimflag = 'X' AND selyn = 'X'.
      IF dt_output-qshkzg = 'S'.
        l_dmbtr = l_dmbtr + dt_output-qdmbtr.

      ELSEIF dt_output-qshkzg = 'H'.
        l_dmbtr = l_dmbtr - dt_output-qdmbtr.

      ENDIF.
    ENDLOOP.
    IF l_dmbtr <> 0.
      MESSAGE e888(sabapdocu) WITH
      '取消的金额不匹配，请检查要取消的数据！'.
      STOP.
    ELSEIF l_dmbtr = 0.
      UPDATE zdimbsbs FROM TABLE dt_bsas.
      IF sy-subrc = 0.
        MESSAGE s888(sabapdocu) WITH '选择的数据成功取消！'.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      DELETE dt_output WHERE selyn = 'X'.
    ENDIF.
  ELSEIF l_answer = 2.
    MESSAGE s888(sabapdocu) WITH '操作被取消！'.
  ENDIF.

ENDFORM.                    " cancel_belnr
*&---------------------------------------------------------------------*
*&      Form  deal_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deal_data .
CLEAR flg_ind.
  "PERFORM f_lock_zbsbs.
  "PERFORM f_lock_zpwsbk.
  IF flg_ind NE 'X'.
    PERFORM reset_data.
  ENDIF.
  CALL FUNCTION 'DEQUEUE_ALL'.  "解锁
  LOOP AT dt_output WHERE selyn = 'X'.
    DELETE dt_output.
  ENDLOOP.

ENDFORM.                    " deal_data
*&---------------------------------------------------------------------*
*&      Form  f_check_cancel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_cancel .
DATA : l_dmbtr1 LIKE glt0-hslvt,
         l_dmbtr2 LIKE glt0-hslvt.
  dt_bank[] = lt_bank.
  dt_bsas[] = lt_bsas.
  LOOP AT dt_bank.
    READ TABLE dt_output WITH KEY zdimcid = dt_bank-zdimcid
                                  zdimline = dt_bank-zdimline
                                  selyn  = 'X'.

    IF sy-subrc = 0.
      IF dt_bank-shkzg = 'S' AND dt_bank-zdimtype = 3.

        l_dmbtr1 = l_dmbtr1 + dt_bank-dmbtr.
      ELSEIF dt_bank-shkzg = 'H' AND dt_bank-zdimtype = 3.

        l_dmbtr1 = l_dmbtr1 - dt_bank-dmbtr.
      ENDIF.
      CLEAR: dt_bank-zdimflag, dt_bank-zdimtype, dt_bank-zdimindie,
             dt_bank-belnr, dt_bank-buzei,
             dt_bank-GJAHR1,
             dt_bank-MONAT1.


      MODIFY dt_bank.
    ENDIF.
  ENDLOOP.

  LOOP AT dt_bsas.
    READ TABLE dt_output WITH KEY belnr = dt_bsas-belnr
                                  buzei = dt_bsas-buzei
                                  selyn  = 'X'.

    IF sy-subrc = 0.
      IF dt_bsas-shkzg = 'S' AND dt_bsas-zdimtype = 3.

        l_dmbtr2 = l_dmbtr2 + dt_bsas-dmbtr.
      ELSEIF dt_bsas-shkzg = 'H' AND dt_bsas-zdimtype = 3.

        l_dmbtr2 = l_dmbtr2 - dt_bsas-dmbtr.
      ENDIF.
      CLEAR: dt_bsas-zdimflag, dt_bsas-zdimtype, dt_bsas-zdimindie,
      dt_bsas-zdimline, dt_bsas-zdimcid, dt_bsas-gjahr1, dt_bsas-monat1,
      dt_bsas-ZDIMCABEL.""
      MODIFY dt_bsas.
    ENDIF.
  ENDLOOP.
* 检查金额是否匹配
  l_dmbtr2 = 0 - l_dmbtr2.
  IF l_dmbtr1 <> l_dmbtr2.
    MESSAGE w888(sabapdocu) WITH
      '取消对账金额不匹配，请检查或选定所有手工对账记录！'.
  ENDIF.

ENDFORM.                    " f_check_cancel
*&---------------------------------------------------------------------*
*&      Form  reset_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_data .
* 更新数据库
  UPDATE zdimwsbk FROM TABLE dt_bank.
  IF sy-subrc = 0.
    UPDATE zdimbsbs FROM TABLE dt_bsas.
    IF sy-subrc = 0.
      MESSAGE s888(sabapdocu) WITH '对帐已经被取消！'.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " reset_data
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page USING head_document TYPE REF TO
cl_dd_document.

DATA: text TYPE sdydo_text_element.

  text =  '查询/取消/锁定对账单程序'.
  CALL METHOD head_document->add_text
    EXPORTING
      text      = text
      sap_style = 'HEADING'.
  CALL METHOD head_document->new_line.

  text = '公司代码：'.
  CALL METHOD head_document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Strong'.

  text = p_bukrs.
  CALL METHOD head_document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Normal'.
  CALL METHOD head_document->add_gap
    EXPORTING
      width = 2.

  text = '会计科目：'    .
  CALL METHOD head_document->add_text
    EXPORTING
      text      = text
      sap_style = 'KEY'.
  text = p_hkont.
  CALL METHOD head_document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Normal'.
  CALL METHOD head_document->add_gap
    EXPORTING
      width = 2.

  text = '年度：'    .
  CALL METHOD head_document->add_text
    EXPORTING
      text      = text
      sap_style = 'KEY'.
  text = p_gjahr.
  CALL METHOD head_document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Normal'.
  CALL METHOD head_document->add_gap
    EXPORTING
      width = 2.
  text = '期间：'    .
  CALL METHOD head_document->add_text
    EXPORTING
      text      = text
      sap_style = 'KEY'.
  text = p_monat.
  CALL METHOD head_document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Normal'.

ENDFORM.                    " top_of_page
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
    AND zdimflag2 = 'X'.
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
    read table RETURN_TAB index 1.
    P_ZUSCOD = RETURN_TAB-fieldval.
  endif.
ENDFORM.                    " FRM_GET_zdimuscod
*&---------------------------------------------------------------------*
*&      Form  f_lock_zbsbs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lock_zbsbs .
CALL FUNCTION 'ENQUEUE_EZZBSBS'
    EXPORTING
      mode_zbsbs     = 'X'
      mandt          = sy-mandt
      bukrs          = p_bukrs
      hkont          = p_hkont
      gjahr          = p_gjahr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE i888(sabapdocu) WITH  'ERROR:会计凭证对帐状态表锁表失败'.
    flg_ind = 'X'.
  ENDIF.

ENDFORM.                    " f_lock_zbsbs
