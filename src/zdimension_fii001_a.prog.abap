*----------------------------------------------------------------------*
* Program Name          :  银行对账单上传与维护程序                    *
* Purpose               :                                              *
* Project Name          :  银行对账项目                                *
* Created by            :  卓成海                                      *
* Create on             :  2009-04-22                                  *
* Functional Consultant :                                              *
* Description           :                                              *
*----------------------------------------------------------------------*
*    Modification Log                                                  *
*Date        Programmer     Corr. #      Description                   *
*                                                                      *
*----------------------------------------------------------------------*

REPORT  ZDIMENSION_FII001_A.
*&---------------------------------------------------------------------*
*& 声明系统字典对象                                                    *
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.
tables: zdimwkpf,            "银行流水账抬头
        t001,
        skb1,
        zdimscnro,
        dd03l,
        zdimwseg,            "银行流水账明细表
        bkpf,               "会计核算凭证标题
        tcurc,              "货币代码
        ZDiMBUHK,          "银行账号与总账科目对照表
        alsmex_tabline,     "具有 Excel 数据的表行
        ska1,               "总帐科目主记录 (科目表)
        zdimwsbk,            "银行流水账已清项
        zdimglt0,           "银行期末余额
        zdimblock,          "月份锁定功能
        ZDIMLDGRP,
        ZDIMBTPLH,          "银行对帐单导入模板表头
        ZDIMBTPLI.           "银行对帐单导入模板行项目
TYPES:BEGIN OF typ_bsis,
        mandt LIKE bsis-mandt,
        bukrs LIKE bsis-bukrs,  "公司代码
        hkont LIKE bsis-hkont,  "会计科目
        zdimuscod like zdimbuhk-ZDIMUSCOD,
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
        belnr LIKE bsis-belnr,
        buzei LIKE bsis-buzei,
      END OF typ_bsis_status.
*&---------------------------------------------------------------------*
*& 声明全局变量
*&---------------------------------------------------------------------*
DATA it_bsas TYPE typ_bsis_status OCCURS 0 WITH HEADER LINE. "会计凭证
DATA: BEGIN OF iexcel OCCURS 0.
        INCLUDE STRUCTURE alsmex_tabline.
DATA: END OF iexcel.
DATA: g_tind(4) TYPE n.
DATA: g_zwfeld(19).
FIELD-SYMBOLS: <fs1>.
DATA:g_flag TYPE c .
DATA:g_ZDiMCID LIKE zdimwseg-ZDiMCID.   "ID号.
DATA:ok_code TYPE  sy-ucomm.
DATA g_flg_lock TYPE c.
DATA g_flg_ind TYPE c.
DATA g_flg_save TYPE c.

DATA:g_flag02 TYPE c."判断是维护或者上载标识
DATA:g_msgtxt TYPE string."输出消息变量
DATA:g_data_check(1) ."检查是否通过标志，当为X时方可保存
*-----------------EXCEL上传表头数据-----------------------------*
DATA:e_bukrs TYPE bukrs,"公司代码
     e_bankn(20) TYPE c,"银行账号
     e_gjahr TYPE gjahr,"会计年度
     e_monat TYPE monat."会计期间
DATA:l_bankn(20) TYPE c."和科目对账单对应银行账号
DATA:l_repid TYPE sy-repid.
DATA:wa_zdimwseg like zdimwseg.
DATA:n_zdimcid LIKE zdimwseg-zdimcid,  "银行对账单ID号
    n_zdimline LIKE zdimwseg-zdimline,"银行对账单行号
    n_zdimname LIKE zdimwseg-zdimname,  "对方账户
    n_zdimcscod LIKE zdimwseg-zdimcscod,  "对方账号
    n_waers LIKE zdimwseg-waers,  "币 种
    n_shkzg LIKE zdimwseg-shkzg,  "借贷标识
    n_dmbtr LIKE zdimwseg-dmbtr,  "金____额
    n_zdimdtext LIKE zdimwseg-zdimdtext . "摘要
*-------------借贷标识帮助内表-----------------*
DATA: BEGIN OF values_tab1 OCCURS 0 ,"
      shkzg LIKE bsis-shkzg ,"借贷标识
      txt   TYPE char50 ,"描述
      END OF values_tab1.
data: begin of it_zuscod occurs 0,
        BUKRS like ZDIMBUHK-bukrs,
        HKONT like ZDiMBUHK-hkont,
        TXT50 like ZDiMBUHK-txt50,
        ZDIMUSCOD like ZDiMBUHK-zdimuscod,
        uscodTXT  like ZDiMBUHK-txt50,
      end of it_zuscod.
DATA:BEGIN OF itab OCCURS 0 ."EXCEL上传明细数据
DATA: edit TYPE c .
        INCLUDE STRUCTURE  zdimwseg.
DATA:END OF  itab.
DATA g_txt50(50) TYPE c.
data: g_fieldname like dd03l-fieldname.
DATA:wa_itab TYPE zdimwseg  ."ITAB结构
DATA itab1 LIKE TABLE OF wa_itab WITH HEADER LINE.
DATA it_zdimwsbk LIKE TABLE OF zdimwsbk WITH HEADER LINE.
DATA:it_chk_tab LIKE STANDARD TABLE OF itab WITH HEADER LINE.
DATA: it_zdimwseg LIKE TABLE OF zdimwseg WITH HEADER LINE.
*DATA: it_zdimwsbk LIKE TABLE OF zdimwsbk WITH HEADER LINE.
DATA: it_zdimglt0 LIKE TABLE OF zdimglt0 WITH HEADER LINE.
DATA g_hkont LIKE bsis-hkont.
DATA it_bsis_st TYPE TABLE OF ZDIMBSBS  WITH HEADER LINE.
DATA it_bsis TYPE typ_bsis OCCURS 0 WITH HEADER LINE.        "会计凭证
data:  it_zdimbtplh like table  of ZDIMBTPLH with header line,          "银行对帐单导入模板表头
       it_ZDIMBTPLI like table of  ZDIMBTPLI with header line.           "银行对帐单导入模板行项目
************************************************************************
* Some variable for alv     *
************************************************************************
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
*----------------------------------------------------------------------*
*         S E L E C T I O N S    &    P A R A M E T E R S
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS:p_bukrs LIKE zdimwseg-bukrs OBLIGATORY, " MEMORY ID buk,
*           p_hkont LIKE bsis-hkont OBLIGATORY,
           p_ZUSCOD LIKE ZDIMBUHK-ZDIMUSCOD OBLIGATORY,
           p_gjahr LIKE zdimwseg-gjahr OBLIGATORY DEFAULT sy-datum+0(4),
           p_monat LIKE zdimwseg-monat OBLIGATORY DEFAULT sy-datum+4(2),
*           p_LDGRP like bkpf-LDGRP  modif  id B2,
           P_filenm  LIKE rlgrap-filename MODIF ID b1.
SELECTION-SCREEN END OF BLOCK bl2.
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_rad1 RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND scr,
            p_rad2 RADIOBUTTON GROUP rd,
            p_rad3 RADIOBUTTON GROUP rd.
SELECTION-SCREEN END OF BLOCK blk1.
************************************************************************
*                      INITIALIZATION                                  *
************************************************************************
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

************************************************************************
*                      AT SCREEN-SCREEN                                *
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'B1'.
        IF p_rad2 = 'X'.
          screen-active = '1'.
          screen-intensified = '1'.
          MODIFY SCREEN.
        ELSE.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
*      when 'B2'.
*        IF p_rad3 = 'X'.
*          screen-active = '1'.
*          screen-intensified = '1'.
*          MODIFY SCREEN.
*        else.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        endif.
    ENDCASE.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_filenm.
  PERFORM FRM_GET_FILENAME USING P_FILENM.

at SELECTION-SCREEN on VALUE-REQUEST FOR p_ZUSCOD.
  perform FRM_GET_zdimuscod using p_ZUSCOD.
  "权限检查

AT SELECTION-SCREEN.
  PERFORM frm_authcheck.
************************************************************************
*              S T A R T - O F - S E L E C T I O N                     *
************************************************************************
START-OF-SELECTION.
  CLEAR g_flag.
  clear: g_flg_lock ,
         g_flg_ind ,
         g_flg_save ,
         g_flag02,
         g_msgtxt,
         g_data_check.
  IF  p_rad1 = 'X'."如果选择维护
    REFRESH itab.
    PERFORM frm_getdata.
    g_flag = 'Y'.
    PERFORM data_out.
  ELSEIF p_rad2 = 'X' ."如果选择上传
    REFRESH itab.
    PERFORM frm_get_excel.   "上载Excel格式的银行对帐单
*    PERFORM fill_itab.   "填充银行对账单明细
*    PERFORM check_excel. "检查内表行是否应该拒绝、或是覆盖、更改。

    g_flag = 'X'.
    PERFORM data_out.
  ELSEIF p_rad3 = 'X'.   "update the accounts docu.
    PERFORM FRM_check_monat.
    PERFORM FRM_f1_query_bsis.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILENM  text
*----------------------------------------------------------------------*
FORM FRM_GET_FILENAME  USING    P_FILENM LIKE rlgrap-filename.
  CALL FUNCTION 'WS_FILENAME_GET'
     EXPORTING
*     DEF_FILENAME           = ' '
*     DEF_PATH               = ' '
*      mask                   = ',*.* ,*.*.'
         mask             = ',Excel文件 (*.xls),*.XLS.'
        mode                   = '0'
        title                  = '上传数据'
     IMPORTING
        filename               = P_FILENM
*     RC                     =
     EXCEPTIONS
       inv_winsys             = 1
       no_batch               = 2
       selection_cancel       = 3
       selection_error        = 4
       OTHERS                 = 5
              .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  frm_authcheck
*&---------------------------------------------------------------------*
*       权限检查
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_authcheck .
  DATA: BEGIN OF i_bukrs OCCURS 0,
           bukrs LIKE t001k-bukrs,
           werks LIKE t001k-bwkey,"评估范围
        END OF i_bukrs.
  DATA:g_str TYPE string.
  DATA : msg TYPE string.
*检查银行帐号
  select single hkont into g_hkont
    from ZDIMBUHK
    where bukrs = p_bukrs
      and ZDIMUSCOD = p_ZUSCOD.
  if sy-subrc ne 0.
    MESSAGE e888(sabapdocu) WITH text-111.
  endif.
**检查分类帐组
*  if p_LDGRP is initial and p_rad3 eq 'X'.
*    message i888(sabapdocu) with text-112.
*  else.
*  endif.
**   检验用户是否有该公司代码的权限      *
*  AUTHORITY-CHECK OBJECT 'Z_FI_BANK' ID 'BUKRS' FIELD p_bukrs
*                                     ID 'SAKNR' FIELD p_hkont.
**                                     ID 'ACTVT' field '03'.
*  IF sy-subrc <> 0.
*
**    CONCATENATE '你没有公司代码' p_bukrs
**                '及其对应科目' p_hkont '的权限！' INTO msg.
*    MESSAGE e888(sabapdocu) WITH msg.
*  ENDIF.
  SELECT bukrs
  INTO TABLE i_bukrs
  FROM t001
  WHERE bukrs = p_bukrs.
  IF LINES( i_bukrs ) = 0.
    MESSAGE '请重新选择条件，不存在公司条件' TYPE 'I'.
    STOP.
  ELSE.
    LOOP AT i_bukrs.
*      AUTHORITY-CHECK OBJECT 'Z_AUTH_CHK'
*               ID 'BUKRS' FIELD i_bukrs-bukrs
**               ID 'WERKS' FIELD i_bukrs-werks
**               ID 'VKORG' FIELD '*'
**               ID 'VTWEG' FIELD '*'
**               ID 'EKORG' FIELD '*'
      .
      g_str = i_bukrs-bukrs.
      IF sy-subrc NE 0.
        CONCATENATE  '没有' g_str '公司的权限' INTO g_str.
        MESSAGE g_str TYPE 'I'.
        STOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF p_monat < 1 OR p_monat > 12 .
    MESSAGE e888(sabapdocu) WITH '请输入正确的会计期间！'.
  ENDIF.

  SHIFT g_hkont RIGHT DELETING TRAILING space.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = g_hkont
    IMPORTING
      output = g_hkont.
* 检查会计科目
  CLEAR ska1.
  SELECT SINGLE xspeb FROM ska1 INTO ska1-xspeb WHERE saknr = g_hkont.
  IF ska1-xspeb = 'X'.
    MESSAGE e888(sabapdocu) WITH '会计科目已冻结，不能上传银行对帐单！'
.
  ENDIF.
  SELECT SINGLE txt50 FROM skat INTO g_txt50 WHERE saknr = g_hkont.

* 检查会计期间是否冻结
  PERFORM frm_check_monat.
ENDFORM.                    " frm_authcheck
*&---------------------------------------------------------------------*
*&      Form  frm_check_monat
*&---------------------------------------------------------------------*
*       检查当前的期间是否可用
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_check_monat .
  DATA l_monat LIKE bsis-monat.
  DATA l_gjahr LIKE bsis-gjahr.
  DATA i_monat TYPE TABLE OF zdimblock WITH HEADER LINE.
  REFRESH i_monat.
  IF p_monat = '01'.
    l_monat = '12'.
    l_gjahr = p_gjahr - 1.
    SELECT * FROM zdimblock INTO TABLE i_monat
      WHERE bukrs = p_bukrs  AND
            gjahr = p_gjahr  AND
            hkont = g_hkont  AND
            ZDIMUSCOD = p_ZUSCOD and
            monat = p_monat.

    SELECT * FROM zdimblock APPENDING TABLE i_monat
      WHERE bukrs = p_bukrs  AND
            gjahr = l_gjahr  AND
            hkont = g_hkont  AND
            ZDIMUSCOD = p_ZUSCOD and
            monat = l_monat.
  ELSE.
    l_monat = p_monat - 1.
    SELECT * FROM zdimblock INTO TABLE i_monat
      WHERE bukrs = p_bukrs  AND
            gjahr = p_gjahr  AND
            hkont = g_hkont  AND
            ZDIMUSCOD = p_ZUSCOD and
            ( monat = l_monat OR monat = p_monat ).
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
             ZDIMUSCOD = p_ZUSCOD .
    IF sy-subrc = 0.
      MESSAGE i888(sabapdocu) WITH '请选择正确的期间！'.
    ELSE.

    ENDIF.
  ENDIF.
ENDFORM.                    " frm_check_monat
*&---------------------------------------------------------------------*
*&      Form  frm_getdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_getdata .
  CLEAR zdimbuhk.
  SELECT SINGLE * FROM zdimbuhk
      WHERE bukrs = p_bukrs AND
            hkont = g_hkont and
            ZDIMUSCOD = p_ZUSCOD.
  IF sy-subrc = 0.
    SELECT
           b~bukrs
           b~gjahr
           b~monat
           b~ZDIMCID
           b~ZDIMLINE
           b~ZDIMUSCOD
           b~ZDIMEDATE
           b~ZDIMNAME
           b~ZDIMCSCOD
           b~waers
           b~shkzg
           b~dmbtr
           b~ZDiMDTEXT
           b~ZDIMFLAG
           b~UNAME
           b~AEDTM
         INTO CORRESPONDING FIELDS OF TABLE itab
         FROM zdimwsbk as a INNER JOIN zdimwseg as b  ON
                   a~bukrs = b~bukrs AND
                   a~gjahr = b~gjahr AND
                   a~ZDIMCID = b~ZDIMCID AND
                   a~ZDIMLINE = b~ZDIMLINE
         WHERE a~bukrs = p_bukrs
          AND a~gjahr = p_gjahr
          AND a~monat = p_monat
          AND a~ZDIMUSCOD = zdimbuhk-zdimuscod
          AND a~ZDIMFLAG NE 'X'.
  ENDIF.
ENDFORM.                    " frm_getdata
*&---------------------------------------------------------------------*
*&      Form  data_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_out .
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
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      it_events                   = git_events[]
    TABLES
      t_outtab                    = itab  "输出的内表
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " data_out
*&---------------------------------------------------------------------*
*&      Form  fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat .
  PERFORM e01_fieldcat_init USING :
  'EDIT'   '选择'         4  'X' '' ''  ''  'X' ' ' ''       '' 'X',
  'ZDIMCID'  '银行对账号'   10 ''  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'ZDIMLINE' '银行对账行号' 12 ''  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'BUKRS' '公司代码'      8  ''  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'GJAHR' '会计年度'      8  ''  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'MONAT' '期间'          4  ''  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'ZDIMUSCOD' '银行帐户号码'  20 'X' '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'ZDIMEDATE' '交易日期'      10 'X' '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'ZDIMNAME' '对方户名'      24 'X' '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'ZDIMCSCOD' '对方帐号'      20 'X' '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'WAERS' '货币类型'      8  'X' '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'SHKZG' '借贷标识'      8  'X' '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'DMBTR' '金额'          23 '' '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' '',
  'ZDIMDTEXT' '摘要'          30 '' '' 'X'  ''  '' ' ' 'ZDIMWSEG' '' '',
  'ZDIMFLAG' '完成标志'      8  ''  '' 'X' ''  ''  ' ' 'ZDIMWSEG' '' ''.
ENDFORM.                    " fieldcat
*&---------------------------------------------------------------------*
*&      Form  e01_fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1080   text
*      -->P_1081   text
*      -->P_4      text
*      -->P_1083   text
*      -->P_1084   text
*      -->P_1085   text
*      -->P_1086   text
*      -->P_1087   text
*      -->P_1088   text
*      -->P_1089   text
*      -->P_1090   text
*      -->P_1091   text
*----------------------------------------------------------------------*
FORM e01_fieldcat_init  USING
 field_name      TYPE c
 field_text      TYPE c
 field_lenth     TYPE i
 field_edit      TYPE c
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
*  ls_fieldcat-round = 1.
  ls_fieldcat-edit = field_edit.
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
*---------------------------------------------------------------------*
*       FORM html_top_of_page                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DOCUMENT                                                      *
*---------------------------------------------------------------------*
FORM html_top_of_page USING document TYPE REF TO cl_dd_document.

  DATA: text TYPE sdydo_text_element.

  text = '公司代码: '.
  CALL METHOD document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Strong'.

  text = p_bukrs.
  CALL METHOD document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Strong'.

  CALL METHOD document->add_gap
    EXPORTING
      width = 1.
ENDFORM.                    "html_top_of_page
*&---------------------------------------------------------------------*
*&      Form  FRM_UCOMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_UCOMM    text
*      -->F_SELFIELD text
*----------------------------------------------------------------------*
FORM frm_ucomm USING f_ucomm    LIKE sy-ucomm
                     f_selfield TYPE slis_selfield.
  DATA:l_flag(1) .
  CLEAR G_msgtxt.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  CALL METHOD lr_grid->check_changed_data.
  f_selfield-refresh = 'X'.

  CASE f_ucomm.
    WHEN '&IC1'.
    WHEN 'APP'.
      CALL SCREEN 9000 STARTING AT 10 5.
    WHEN 'DATA_SAVE'.
      if p_rad2 eq 'X'.
        PERFORM createnum.   "创建银行对账单流水ID
        PERFORM frm_f2_save.
        p_rad1 = 'X'.
        CLEAR p_rad2.
        G_flag = 'Y'.
      endif.
    WHEN 'ALL'.
      LOOP AT itab.
        itab-edit = 'X'.
        MODIFY itab.
      ENDLOOP.
    WHEN 'SAL'.
      LOOP AT itab.
        itab-edit = ' '.
        MODIFY itab.
      ENDLOOP.
    WHEN 'DEL'."删除选择行
      PERFORM delevent.
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
  if p_rad2 <> 'X'.
    SET PF-STATUS 'ZSTANDARD_FULLSCREEN' EXCLUDING 'SAVE_DATA'.         "EXCLUDING RT_EXTAB.
  ELSE.
    SET PF-STATUS 'ZSTANDARD_FULLSCREEN'.
  ENDIF.
ENDFORM.                               " SET_STATUS2

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_get_event .
  DATA: formname_top_of_page TYPE slis_formname VALUE
'FRM_TOP_OF_PAGE1',
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
*---------------------------------------------------------------------*
*       FORM DELEVENT                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM delevent .
  IF G_flag = 'X'.   "上载
    LOOP AT itab WHERE  edit = 'X'.
      PERFORM  exceldelete.
    ENDLOOP.
  ELSEIF G_flag = 'Y'.
    PERFORM FRM_f_lock.
    IF G_flg_ind NE 'X'.
      PERFORM maintdelete.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_ALL'.  "解锁
  ENDIF.
ENDFORM.                    "delevent
*---------------------------------------------------------------------*
*       FORM EXCELdel                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM exceldelete .
*  DELETE FROM zpwseg WHERE bukrs = itab-bukrs
*                      AND  gjahr = itab-gjahr
*                      AND  monat = itab-monat
*                      AND  waste = itab-waste
*                      AND  intnum = itab-intnum.
*  DELETE FROM zpwsbk WHERE bukrs = itab-bukrs
*                          AND  gjahr = itab-gjahr
*                          AND  monat = itab-monat
*                          AND  waste = itab-waste
*                          AND  intnum = itab-intnum.
**  PERFORM f2_deal_zpglt0.
  DELETE itab.
ENDFORM.                    "exceldelete
*---------------------------------------------------------------------*
*       FORM maintdelete,                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM maintdelete.
  REFRESH IT_zdimglt0.
  SELECT * FROM zdimglt0 INTO TABLE it_zdimglt0
      WHERE bukrs = p_bukrs AND
              hkont = g_hkont AND
              gjahr = p_gjahr.
  LOOP AT itab WHERE  edit = 'X'.
    IF itab-ZDIMFLAG NE 'X' ."如果未找到，那么执行删除动作
      DELETE FROM zdimwseg WHERE bukrs = itab-bukrs
                        AND  gjahr = itab-gjahr
                        AND  monat = itab-monat
                        AND  ZDIMCID = itab-ZDIMCID
                        AND  ZDIMLINE = itab-ZDIMLINE.
      DELETE FROM zdimwsbk WHERE bukrs = itab-bukrs
                        AND  gjahr = itab-gjahr
                        AND  monat = itab-monat
                        AND  ZDIMCID = itab-ZDIMCID
                        AND  ZDIMLINE = itab-ZDIMLINE.
      PERFORM f2_deal_zpglt0.
      DELETE itab.
      MESSAGE s888(sabapdocu) WITH '数据删除成功'.
    ELSE.
      CONCATENATE '银行对账单:' itab-ZDIMCID ',行号:' itab-ZDIMLINE
                  '已经对账，不能删除' INTO g_msgtxt.
      MESSAGE e888(sabapdocu) WITH g_msgtxt.
    ENDIF.
    MODIFY zdimglt0 FROM TABLE it_zdimglt0.
  ENDLOOP.
ENDFORM.                    "maintdelete
*&---------------------------------------------------------------------*
*&      Form  f2_deal_zpglt0 处理月结数据
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f2_deal_zpglt0.
  CLEAR it_zdimglt0.
  it_zdimglt0-mandt = sy-mandt.
  it_zdimglt0-bukrs = itab-bukrs.
  it_zdimglt0-hkont = g_hkont.
*  it_zdimglt0-txt50 = g_txt50.
  it_zdimglt0-ZDIMUSCOD = itab-ZDIMUSCOD.
  it_zdimglt0-gjahr = p_gjahr.
  it_zdimglt0-shkzg = itab-shkzg.
  CLEAR zdimwkpf.
  zdimwkpf-mandt = sy-mandt.
  zdimwkpf-bukrs = p_bukrs.
  zdimwkpf-ZDIMCID = g_ZDIMCID.

  zdimwkpf-ZDIMEDATE = sy-datum.
  zdimwkpf-erdat = sy-datum.
  zdimwkpf-ernam = sy-uname.
  CASE p_monat.
    WHEN '01'.
      it_zdimglt0-hsl01 = - itab-dmbtr.
    WHEN '02'.
      it_zdimglt0-hsl02 = - itab-dmbtr.
    WHEN '03'.
      it_zdimglt0-hsl03 = - itab-dmbtr.
    WHEN '04'.
      it_zdimglt0-hsl04 = - itab-dmbtr.
    WHEN '05'.
      it_zdimglt0-hsl05 = - itab-dmbtr.
    WHEN '06'.
      it_zdimglt0-hsl06 = - itab-dmbtr.
    WHEN '07'.
      it_zdimglt0-hsl07 = - itab-dmbtr.
    WHEN '08'.
      it_zdimglt0-hsl08 = - itab-dmbtr.
    WHEN '09'.
      it_zdimglt0-hsl09 = - itab-dmbtr.
    WHEN '10'.
      it_zdimglt0-hsl10 = - itab-dmbtr.
    WHEN '11'.
      it_zdimglt0-hsl11 = - itab-dmbtr.
    WHEN '12'.
      it_zdimglt0-hsl12 = - itab-dmbtr.
    WHEN '13'.
      it_zdimglt0-hsl13 = - itab-dmbtr.
    WHEN '14'.
      it_zdimglt0-hsl14 = - itab-dmbtr.
    WHEN '15'.
      it_zdimglt0-hsl15 = - itab-dmbtr.
    WHEN '16'.
      it_zdimglt0-hsl16 = - itab-dmbtr.
  ENDCASE.
  COLLECT it_zdimglt0.
ENDFORM.                    " f2_deal_zpglt0
*&---------------------------------------------------------------------*
*&      Form  frm_f2_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_f2_save .
  FREE itab1.
*  REFRESH i_zpglt0.
  PERFORM frm_f1_query_zdwmglt0.
  LOOP AT itab.
    CLEAR itab1.
    MOVE-CORRESPONDING itab TO itab1.
    itab1-ZDIMCID = itab-ZDIMCID.
    itab1-ZDIMLINE = itab-ZDIMLINE.
    APPEND itab1.
    MOVE-CORRESPONDING itab1 TO it_zdimwsbk.
    it_zdimwsbk-hkont = g_hkont.
    APPEND it_zdimwsbk.
*    itab-waste = l_waste.
*    itab-intnum = sy-tabix.
    CLEAR it_zdimglt0.
    it_zdimglt0-mandt = sy-mandt.
    it_zdimglt0-bukrs = itab-bukrs.
    it_zdimglt0-hkont = g_hkont.
*    it_zdimglt0-txt50 = g_txt50.
    it_zdimglt0-zdimuscod = itab-zdimuscod.
    it_zdimglt0-gjahr = p_gjahr.
    it_zdimglt0-shkzg = itab-shkzg.
    CLEAR zdimwkpf.
    zdimwkpf-zdimuscod = l_bankn.
    zdimwkpf-mandt = sy-mandt.
    zdimwkpf-bukrs = p_bukrs.
    zdimwkpf-ZDIMCID = itab-ZDIMCID.
    zdimwkpf-ZDIMEDATE = sy-datum.
    zdimwkpf-erdat = sy-datum.
    zdimwkpf-ernam = sy-uname.
    CASE p_monat.
      WHEN '01'.
        it_zdimglt0-hsl01 = itab-dmbtr.
      WHEN '02'.
        it_zdimglt0-hsl02 = itab-dmbtr.
      WHEN '03'.
        it_zdimglt0-hsl03 = itab-dmbtr.
      WHEN '04'.
        it_zdimglt0-hsl04 = itab-dmbtr.
      WHEN '05'.
        it_zdimglt0-hsl05 = itab-dmbtr.
      WHEN '06'.
        it_zdimglt0-hsl06 = itab-dmbtr.
      WHEN '07'.
        it_zdimglt0-hsl07 = itab-dmbtr.
      WHEN '08'.
        it_zdimglt0-hsl08 = itab-dmbtr.
      WHEN '09'.
        it_zdimglt0-hsl09 = itab-dmbtr.
      WHEN '10'.
        it_zdimglt0-hsl10 = itab-dmbtr.
      WHEN '11'.
        it_zdimglt0-hsl11 = itab-dmbtr.
      WHEN '12'.
        it_zdimglt0-hsl12 = itab-dmbtr.
      WHEN '13'.
        it_zdimglt0-hsl13 = itab-dmbtr.
      WHEN '14'.
        it_zdimglt0-hsl14 = itab-dmbtr.
      WHEN '15'.
        it_zdimglt0-hsl15 = itab-dmbtr.
      WHEN '16'.
        it_zdimglt0-hsl16 = itab-dmbtr.
    ENDCASE.
    IF g_flg_save <> 'X'.
      COLLECT it_zdimglt0.
    ENDIF.
  ENDLOOP.
  PERFORM frm_f_lock.
  IF g_flg_ind NE 'X'.
    MODIFY zdimwseg FROM TABLE itab1.
    IF sy-subrc = 0.
      MODIFY zdimwsbk FROM TABLE it_zdimwsbk.
      IF p_rad1 = 'X'.
        MESSAGE s888(sabapdocu) WITH '维护记录已经更新到数据库！'.
      ENDIF.
      IF sy-subrc = 0.
        IF p_rad2 = 'X'.
          MODIFY zdimglt0 FROM TABLE it_zdimglt0.
          IF sy-subrc = 0.
            MODIFY zdimwkpf.
            MESSAGE s888(sabapdocu) WITH '上传数据已经更新到数据库！'.
            COMMIT WORK AND WAIT.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  CALL FUNCTION 'DEQUEUE_ALL'.  "解锁
  REFRESH : itab1,it_zdimwsbk,it_zdimglt0.
  CLEAR : zdimwkpf.
  g_flg_save = 'X'.
ENDFORM.                    " frm_f2_save
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
  l_monat = p_monat.
  SELECT SINGLE  * FROM zdimglt0
      WHERE bukrs = p_bukrs AND
            hkont = g_hkont AND
            ZDIMUSCOD = p_zuscod and
            gjahr = p_gjahr.
  IF sy-subrc = 0.  "如果一月份不是第一次对帐，就不用再取上一年的记录
    CLEAR l_monat.
  ENDIF.
  IF l_monat = '01'.
    l_gjahr = p_gjahr - 1.
  ELSE.
    l_gjahr = p_gjahr.
  ENDIF.
  SELECT * FROM zdimglt0 " INTO TABLE i_zpglt
      WHERE bukrs = p_bukrs AND
            hkont = g_hkont AND
            ZDIMUSCOD = p_zuscod and
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
*&---------------------------------------------------------------------*
*&      Form  FRM_F_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_f_lock.
  IF g_flg_lock = space.
    CLEAR g_flg_ind .
*   PERFORM frm_lock_zdimglt0.
*    PERFORM frm_lock_zdimwseg.
**    PERFORM frm_lock_zdimbsbs.
*    PERFORM frm_lock_zdimwsbk.
*    PERFORM frm_lock_zdimwkpf.
  ENDIF.
  g_flg_lock = 'X'.
ENDFORM.                    "f_lock
*&---------------------------------------------------------------------*
*&      Form  FRM_f1_query_bsis
*&---------------------------------------------------------------------*
*& 取尚未对过帐的会计凭证
*&---------------------------------------------------------------------*
FORM frm_f1_query_bsis.
  DATA: d_bkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE,
        it_bseg like table of bseg with HEADER LINE.
  data: i_flag,
        I_char1(100) ,
        i_char2(150),
        i_char(30).

  FIELD-SYMBOLS: <fs_bsis> like it_bsis,
                 <fs_char> type any.
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
*    and   RLDNR eq zdimldgrp-RLDNR.
  loop at d_bkpf .
    DELETE FROM zdimbsbs WHERE bukrs = it_bsis-bukrs
                        AND gjahr = d_bkpf-stjah
                        AND belnr = d_bkpf-stblg
                        and ZDIMFLAG ne 'X'.
  ENDLOOP.
* 取当前月已对过帐的会计凭证
  SELECT * FROM zdimbsbs
       INTO CORRESPONDING FIELDS OF TABLE it_bsas
       WHERE bukrs = p_bukrs AND
             hkont = g_hkont AND
             gjahr = p_gjahr AND
             monat = p_monat AND
             ZDIMUSCOD = p_ZUSCOD and
             ZDIMFLAG = 'X'.
* 删除已对过帐的会计凭证
  SORT it_bsas BY bukrs hkont gjahr belnr buzei.
  LOOP AT it_bsis.
    READ TABLE it_bsas WITH KEY belnr = it_bsis-belnr
                           buzei = it_bsis-buzei
                           BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE it_bsis.    "删除已对过帐的会计凭证
    ENDIF.
  ENDLOOP.
if not it_bsis[] is initial.
  PERFORM frm_update_zdimbsbs.
else.
  message i888(sabapdocu) WITH text-113.
endif.
ENDFORM.                    " FRM_f1_query_bsis
*&---------------------------------------------------------------------*
*&      Form  frm_update_zdimbsbs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_update_zdimbsbs .
  REFRESH it_bsis_st.
  LOOP AT it_bsis.
    MOVE-CORRESPONDING it_bsis TO it_bsis_st .
    it_bsis_st-mandt = sy-mandt.
    APPEND it_bsis_st.
  ENDLOOP.
  CLEAR g_flg_ind.

  CALL FUNCTION 'ENQUEUE_EZDIMBSBS'
   EXPORTING
     MODE_ZDIMBSBS        = 'E'
     MANDT                = SY-MANDT
     BUKRS                = p_bukrs
     HKONT                = g_hkont
     GJAHR                = p_gjahr
*                     X_BUKRS              = ' '
*                     X_HKONT              = ' '
*                     X_GJAHR              = ' '
*                     _SCOPE               = '2'
*                     _WAIT                = ' '
*                     _COLLECT             = ' '
   EXCEPTIONS
     FOREIGN_LOCK         = 1
     SYSTEM_FAILURE       = 2
     OTHERS               = 3
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF sy-subrc = 0.
    MODIFY zdimbsbs FROM TABLE it_bsis_st.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      MESSAGE s888(sabapdocu) WITH  '会计凭证已更新！'.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'DEQUEUE_ALL'.  "解锁
  FREE it_bsis_st.
ENDFORM.                    " frm_update_zdimbsbs
*&---------------------------------------------------------------------*
*&      Form  frm_get_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_excel .
*科目与银行帐号及模板的关系

  DATA: I_CHAR01(30),
        i_insert,
        i_startline type i,
        i_strlin type i,
        i_lin01 type i,
        i_lin02 type i,
        i_dmbtr like bseg-dmbtr,
        i_value type alsmex_tabline-value.

  FIELD-SYMBOLS: <FS_CHAR01>,
                 <fs_iexcel> like  alsmex_tabline.
  select single * from ZDiMBUHK
  where BUKRS  = p_bukrs
    and HKONT  = g_hkont
    and zdimuscod = p_ZUSCOD.

  if sy-subrc eq 0.
    if ZDiMBUHK-ZDIMBTNO is initial.
      message text-101 type 'I'.
      stop.
    else.
      select * into table  it_zdimbtplh
      from zdimbtplh
      where ZDIMBTNO = ZDiMBUHK-ZDIMBTNO.
      select * into table  it_zdimbtpli
     from zdimbtpli
     where ZDIMBTNO = ZDiMBUHK-ZDIMBTNO.
    endif.
  endif.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_filenm
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 50
      i_end_row               = 10000
    TABLES
      intern                  = iexcel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    CLEAR g_flag .
    g_flag  = 'X'.
    WRITE: / 'EXCEL UPLOAD FAILED ', p_filenm, sy-subrc.
    STOP.
  ELSE.
    SORT iexcel BY row col.
*读取表头的数据
    loop at  it_zdimbtplh.
      read table iexcel with key  ROW = it_zdimbtplh-zdimROW
                                 COl = it_zdimbtplh-zdimCol.
      if sy-subrc eq 0.
*      CONDENSE iexcel-VALUE NO-GAPS.
        SHIFT iexcel-VALUE LEFT DELETING LEADING space.
        search iexcel-VALUE for ':'.
        if sy-subrc eq 0.
          SPLIT iexcel-value  at ':' into i_value iexcel-value .
        else.
          search iexcel-VALUE for '：'.
          if sy-subrc eq 0.
            SPLIT iexcel-value  at '：' into i_value iexcel-value .
          endif.
        endif.
        SHIFT iexcel-VALUE LEFT DELETING LEADING space.
        if it_zdimbtplh-ZDIMRFIELD eq 'WAERS'.
*          SELECT SINGLE WAERS INTO ZDIMWKPF-WAERS
*          FROM TCURT
*          WHERE SPRAS = SY-LANGU
*           AND ( KTEXT = iexcel-VALUE
*           OR  LTEXT = iexcel-VALUE ).
*          if sy-subrc ne 0.
*            MESSAGE text-102 TYPE 'I'.
*            stop.
*          endif.
        ELSE.
          CONCATENATE 'ZDIMWKPF-'  it_zdimbtplh-ZDIMRFIELD INTO I_CHAR01.
          ASSIGN (I_CHAR01) TO <FS_CHAR01>.
          <FS_CHAR01> = IEXCEL-VALUE.
        ENDIF.
      endif.
    endloop.
    ZDIMWKPF-BUKRS = p_bukrs.
     select single waers into ZDIMWKPF-waers
      from skb1
      where BUKRS = p_bukrs
        and SAKNR = g_hkont.
*      ZDIMWKPF-GJAHR = p_gjahr.
*       ZDIMWKPF-MONAT = p_monat.
*检查银行帐号：
    if ZDIMWKPF-ZDIMUSCOD ne p_zUSCOD.
      MESSAGE i888(sabapdocu) WITH text-110.
      stop.
    endif.
*读取行项目数据
    read table it_zdimbtpli with key ZDIMRFIELD = 'START'.
    IF SY-SUBRC EQ 0.
      I_STARTLINE = it_zdimbtpli-ZDIMROW.
    else.
      MESSAGE TEXT-1O2 TYPE 'I'.
      stop.
    ENDIF.
    delete iexcel where row < I_STARTLINE.
    clear: itab[],
           itab,
           i_insert.
    loop at iexcel ASSIGNING <fs_iexcel>.

*     SHIFT <fs_iexcel>-VALUE.
*     at new row.
**检查是不是交易数据
*     if <fs_iexcel>-value cn '0123456789.-/ '.
*        i_insert = 'X'.
*     else.
*       clear i_insert.
*     endif.
*     ENDAT.
*     CONDENSE <fs_iexcel>-VALUE NO-GAPS.
      SHIFT <fs_iexcel>-VALUE LEFT DELETING LEADING space.
*   if i_insert is initial.
      read table  it_zdimbtpli with key  zdimcol = <fs_iexcel>-col.
      if sy-subrc eq 0.
*交易日期
        if  it_zdimbtpli-ZDIMRFIELD eq 'ZDIMEDATE'.
*处理日期与时间同时存在的时候
          search <fs_iexcel>-VALUE for ':'.
          if sy-subrc eq 0.
            SPLIT <fs_iexcel>-value  at '' into <fs_iexcel>-value  i_value .
          endif.

          CALL FUNCTION 'ZDIMCONVERT_DATE_INPUT'
            EXPORTING
              VALUE                 = <fs_iexcel>-value
              ZDIMDATFM             = it_zdimbtpli-ZDIMDATFM
            IMPORTING
              OUTPUT                = itab-ZDIMEDATE
            EXCEPTIONS
              WRONG_FORMAT_IN_INPUT = 1
              OTHERS                = 2.
          IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

          ENDIF.

*发生额
        elseif  it_zdimbtpli-ZDIMRFIELD eq 'DMBTR01' or
                it_zdimbtpli-ZDIMRFIELD eq 'DMBTR02'.


          CALL FUNCTION 'ZDIMCONVERT_CURRENCY_INPUT'
            EXPORTING
              VALUE                 = <fs_iexcel>-value
              ZDIMDCPFM             = it_ZDIMBTPLI-ZDIMDCPFM
            IMPORTING
              OUTPUT                = i_dmbtr
            EXCEPTIONS
              WRONG_FORMAT_IN_INPUT = 1
              OTHERS                = 2.
          IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
          if i_dmbtr < 0.
            i_dmbtr = i_dmbtr * -1.
          endif.
          IF i_dmbtr > 0.
*借方或货方
            if it_zdimbtpli-ZDIMRFIELD eq 'DMBTR01'.
              itab-SHKZG = 'S'.
            else.
              itab-SHKZG = 'H'.
            endif.
            itab-DMBTR = i_dmbtr.
          ENDIF.

*其他字段
        else.
*        SHIFT <fs_iexcel>-VALUE.
          CONCATENATE 'ITAB-'  it_zdimbtplI-ZDIMRFIELD INTO I_CHAR01.
          ASSIGN (I_CHAR01) TO <FS_CHAR01>.
          <FS_CHAR01> = <fs_iexcel>-VALUE.
        endif.
      endif.
*      endif.
      AT END OF row.
        IF NOT itab-ZDIMEDATE IS INITIAL.
          itab-BUKRS = p_bukrs.
          ITAB-GJAHR = p_gjahr.
          ITAB-MONAT = p_monat.
          ITAB-ZDIMUSCOD = ZDIMWKPF-ZDIMUSCOD.
          ITAB-WAERS  = ZDIMWKPF-WAERS.
          APPEND ITAB.
          CLEAR itab.
        ENDIF.
      ENDAT.
    endloop.
  endif.
  delete itab where dmbtr eq 0.
ENDFORM.                    " frm_get_excel
*&---------------------------------------------------------------------*
*&      Form  FRM_word_wrap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_CHAR01  text
*----------------------------------------------------------------------*
FORM FRM_word_wrap  CHANGING  data_value TYPE c.
  DATA :BEGIN OF l_data OCCURS 0,
            data(6) TYPE c,
           END OF l_data.
  SPLIT  data_value  AT ',' INTO TABLE l_data.
  CLEAR data_value.
  LOOP AT l_data.
    CONCATENATE data_value l_data-data INTO data_value.
  ENDLOOP.

ENDFORM.                    " FRM_word_wrap
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
  LOOP AT itab.
    itab-zdimcid = i_zdimcid.
    itab-ZDIMLINE = sy-tabix.
    MODIFY itab.
  ENDLOOP.
ENDFORM.                    " createnum
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CLEAR ok_code.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM datainputcheck."输入检查
*      PERFORM data_check."数据检查，主要是判断是否能更新进银行记账单
      PERFORM datafill."保存录入数据，做更新数据库或内表操作
      LEAVE TO SCREEN 0 .
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  datainputcheck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM datainputcheck .
  "changed by d00194 all screen 0100 to 9000 at 090527
  IF  n_zdimcid IS INITIAL.
    MESSAGE i888(sabapdocu) WITH '请选择一条银行流水帐后再进行添加！'.
    LEAVE TO SCREEN 9000.
  ENDIF.
  IF bkpf-bldat IS INITIAL.
    MESSAGE i888(sabapdocu) WITH '交易日期为必需选项'.
    LEAVE TO SCREEN 9000.
  ENDIF.
  IF n_zdimname IS INITIAL.
    MESSAGE w888(sabapdocu) WITH '对方账户为空'.
  ENDIF.
  IF n_zdimcscod  IS INITIAL.
    MESSAGE w888(sabapdocu) WITH '对方账号为空'.
  ENDIF.
  IF bkpf-waers IS INITIAL.
    MESSAGE i888(sabapdocu) WITH '币种为必需选项'.
    LEAVE TO SCREEN 9000.
  ENDIF.
  IF n_shkzg IS INITIAL.
    MESSAGE i888(sabapdocu) WITH '借贷标识为必需选项'.
    LEAVE TO SCREEN 9000.
  ENDIF.
  IF n_dmbtr IS INITIAL.
    MESSAGE i888(sabapdocu) WITH '金额为必需选项'.
    LEAVE TO SCREEN 9000.
  ENDIF.
  IF n_zdimdtext  IS INITIAL.
    MESSAGE w888(sabapdocu) WITH '摘要为空'.
  ENDIF.
ENDFORM.                    " datainputcheck
*&---------------------------------------------------------------------*
*&      Form  datafill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM datafill .
  CLEAR wa_itab.
  wa_itab-mandt = sy-mandt.
  wa_itab-bukrs = p_bukrs."公司代码
  wa_itab-waers = bkpf-waers."货币类型
  wa_itab-zdimcid = n_zdimcid."银行对账单流水账ID
  wa_itab-zdimuscod = e_bankn. "银行帐号
  wa_itab-zdimline = n_zdimline."内部计数器
  wa_itab-gjahr  = p_gjahr." 年度
  wa_itab-monat  = p_monat."月份
  wa_itab-zdimname  = n_zdimname."对方户名
  wa_itab-zdimcscod  = n_zdimcscod."对账账号，对应会计凭证的参考码3
  wa_itab-zdimedate  = bkpf-bldat."交易日期
  wa_itab-shkzg  = n_shkzg."借贷标识
  wa_itab-dmbtr  = n_dmbtr."金额
  wa_itab-zdimdtext  = n_zdimdtext."摘要.

  IF g_flag = 'X'.
    MOVE-CORRESPONDING wa_itab TO itab.
    APPEND itab.
  ELSE.
    REFRESH it_zdimglt0.
    SELECT * FROM zdimglt0 INTO TABLE it_zdimglt0
       WHERE bukrs = p_bukrs AND
              hkont = g_hkont AND
              gjahr = p_gjahr.

    CLEAR it_zdimglt0.
    it_zdimglt0-mandt = sy-mandt.
    it_zdimglt0-bukrs = wa_itab-bukrs.
    it_zdimglt0-hkont = g_hkont.
*    it_zdimglt0-txt50 = g_txt50.
    it_zdimglt0-zdimuscod = wa_itab-zdimuscod.
    it_zdimglt0-gjahr = p_gjahr.
    it_zdimglt0-shkzg = wa_itab-shkzg.
    CASE p_monat.
      WHEN '01'.
        it_zdimglt0-hsl01 = wa_itab-dmbtr.
      WHEN '02'.
        it_zdimglt0-hsl02 = wa_itab-dmbtr.
      WHEN '03'.
        it_zdimglt0-hsl03 = wa_itab-dmbtr.
      WHEN '04'.
        it_zdimglt0-hsl04 = wa_itab-dmbtr.
      WHEN '05'.
        it_zdimglt0-hsl05 = wa_itab-dmbtr.
      WHEN '06'.
        it_zdimglt0-hsl06 = wa_itab-dmbtr.
      WHEN '07'.
        it_zdimglt0-hsl07 = wa_itab-dmbtr.
      WHEN '08'.
        it_zdimglt0-hsl08 = wa_itab-dmbtr.
      WHEN '09'.
        it_zdimglt0-hsl09 = wa_itab-dmbtr.
      WHEN '10'.
        it_zdimglt0-hsl10 = wa_itab-dmbtr.
      WHEN '11'.
        it_zdimglt0-hsl11 = wa_itab-dmbtr.
      WHEN '12'.
        it_zdimglt0-hsl12 = wa_itab-dmbtr.
      WHEN '13'.
        it_zdimglt0-hsl13 = wa_itab-dmbtr.
      WHEN '14'.
        it_zdimglt0-hsl14 = wa_itab-dmbtr.
      WHEN '15'.
        it_zdimglt0-hsl15 = wa_itab-dmbtr.
      WHEN '16'.
        it_zdimglt0-hsl16 = wa_itab-dmbtr.
    ENDCASE.
    COLLECT it_zdimglt0.

    CLEAR it_zdimwsbk.
    MOVE-CORRESPONDING wa_itab TO it_zdimwsbk.
    it_zdimwsbk-hkont = g_hkont.
    INSERT into zdimwseg values wa_itab.
    IF sy-subrc = 0 .
      INSERT into zdimwsbk values it_zdimwsbk.
      IF sy-subrc = 0.
        MODIFY zdimglt0 FROM TABLE it_zdimglt0.
        IF sy-subrc = 0.
          COMMIT WORK .
          CLEAR itab.
          MOVE-CORRESPONDING wa_itab TO itab.
          APPEND itab.
          MESSAGE s888(sabapdocu) WITH '成功插入数据库!'.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK .
    ENDIF.
  ENDIF.
ENDFORM.                    " datafill
*&---------------------------------------------------------------------*
*&      Module  user_command  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  leave to screen 0.
ENDMODULE.                 " user_command  INPUT
*&---------------------------------------------------------------------*
*&      Module  initial  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial OUTPUT.
  n_zdimcid = g_ZDiMCID.
  SORT itab BY zdimcid zdimline DESCENDING.
  READ TABLE itab INDEX 1.
  IF sy-subrc = 0 .
    n_zdimline = itab-zdimline + 1 .
    n_zdimcid = itab-zdimcid.
    e_bankn = itab-zdimuscod.
  ENDIF.
  clear itab.
  select max( zdimline ) into itab-zdimline
    from zdimwseg
    where zdimcid = n_zdimcid.
  if itab-zdimline >= n_zdimline.
    n_zdimline = itab-zdimline + 1.
  endif.
ENDMODULE.                 " initial  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  shkzg_showf4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE shkzg_showf4 INPUT.
  REFRESH values_tab1.
  CLEAR values_tab1.

  l_repid = sy-repid.
  values_tab1-shkzg = 'S'.
  values_tab1-txt = '借方'.
  APPEND values_tab1.CLEAR values_tab1.
  values_tab1-shkzg = 'H'.
  values_tab1-txt = '贷方'.
  APPEND values_tab1.CLEAR values_tab1.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'SHKZG'
      dynpprog    = l_repid
      dynpnr      = sy-dynnr
      dynprofield = 'N_SHKZG'  "屏幕上的名称
      value_org   = 'S'
    TABLES
      value_tab   = values_tab1.
ENDMODULE.                 " shkzg_showf4  INPUT
*&---------------------------------------------------------------------*
*&      Module  shkzg_check_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE shkzg_check_data INPUT.
  IF n_shkzg <> 'S' AND n_shkzg <> 'H'.
    MESSAGE e888(sabapdocu) WITH '请输入正确的借贷标识!'.
    SET CURSOR FIELD 'N_SHKZG'.
  ENDIF.
ENDMODULE.                 " shkzg_check_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  waers_check_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE waers_check_data INPUT.
  SELECT SINGLE * FROM tcurc WHERE waers = bkpf-waers
                             .
  IF sy-subrc <> 0 .
    MESSAGE e888(sabapdocu) WITH '请输入正确的货币类型!'.
  ENDIF.
ENDMODULE.                 " waers_check_data  INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_zdimuscod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ZUSCOD  text
*----------------------------------------------------------------------*
FORM FRM_GET_zdimuscod  USING    P_ZUSCOD like ZDiMBUHK-zdimuscod.
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
