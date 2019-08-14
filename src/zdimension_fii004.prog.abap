*&---------------------------------------------------------------------*
*& Report  ZDIMENSION_FII004
*& Description: 余额初始化程序                                         *
*& Create By  : D00194                                                *
*& Create Date: 2009/04/26
*& Project Name          :  达美银行对账项目

*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDIMENSION_FII004.
DATA:
  ok_code TYPE sy-ucomm.

*INTERNAL TABLE FOR DATA------------------------------------------------*
DATA:
*TC用内表
  BEGIN OF itab OCCURS 0,
    sel,                              "选中标志
    zdimline  TYPE zdimwseg-zdimline,   "计数器
    zdimcid   TYPE zdimwseg-zdimcid,    "银行账单ID
    zdimcscod  TYPE zdimwseg-zdimcscod,     "行项目的参考码(对方ID)
    uname TYPE zdimwseg-uname,     "用户名
    zdimedate TYPE zdimwseg-zdimedate,     "交易日期/凭证日期
    shkzg TYPE zdimwseg-shkzg,
    chgty ,                      "未达类型
    dmbtr TYPE zdimwseg-dmbtr,     "金额
    zdimdtext TYPE zdimwseg-zdimdtext,     "描述
  END OF itab,
*保留用tc表
  BEGIN OF v_itab OCCURS 0,
    sel,
    zdimline TYPE zdimwseg-zdimline,
    zdimcid  TYPE zdimwseg-zdimcid,
    zdimcscod TYPE zdimwseg-zdimcscod,
    uname TYPE zdimwseg-uname,
    zdimedate TYPE zdimwseg-zdimedate,
    shkzg TYPE zdimwseg-shkzg,
    chgty ,
    dmbtr TYPE zdimwseg-dmbtr,
    zdimdtext TYPE zdimwseg-zdimdtext,
  END OF v_itab,
*借贷标识帮助内表
  BEGIN OF values_tab1 OCCURS 0 ,"
    shkzg LIKE bsis-shkzg ,"借贷标识
    txt   TYPE zdimdtext,"zshktxt ,"描述
  END OF values_tab1,
*调节类型帮助内表
  BEGIN OF values_tab2 OCCURS 0 ,"
    chgty TYPE char1 ,"类型
    txt   TYPE zdimdtext,"zchtxt,"描述
  END OF values_tab2.

  .

*END OF INTERNAL TABLE FOR DATA------------------------------------------*

*屏幕用变量
DATA:
  flg_mdf,                              "画面可更改标记
  flg_chg,                              "更改按钮
  flg_lock,                             "锁标记
  v_bukrs TYPE bukrs,                   "保留用
  v_hkont TYPE hkont,                   "保留用
  v_gjahr TYPE gjahr,                   "保留用
  v_monat TYPE rpmax,                   "保留用
  bukrs TYPE zdimbuhk-bukrs,                     "屏幕用
  hkont TYPE zdimbuhk-hkont,
  gjahr TYPE gjahr,
  monat TYPE rpmax,
  uscod TYPE zdimbuhk-zdimuscod,              "银行账号
  txt50 TYPE zdimbuhk-txt50,              "科目描述
  dmbtr2 TYPE zdimwsbk-dmbtr,             "银行余额
  dmbtr1(12) TYPE p DECIMALS 2,         "企业余额
  v_dmbtr2 TYPE zdimwsbk-dmbtr,           "保留画面银行余额
  w_hkont TYPE char10.                  "格式转化用会计科目
*INITIALIZATION

INITIALIZATION.
*TABLES zdimscnro.
*select single *
*  from zdimscnro
*  where ZDIMSCSL eq 'X'.
* if sy-subrc ne 0.
*   message e888(sabapdocu) WITH text-114.
* endif.

CALL SCREEN 0100.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC0100' ITSELF
CONTROLS: TC0100 TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'TC0100'
DATA:     G_TC0100_LINES  LIKE SY-LOOPC.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC0100_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE ITAB LINES TC0100-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TC0100_GET_LINES OUTPUT.
  G_TC0100_LINES = SY-LOOPC.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TC0100_MODIFY INPUT.
  MODIFY ITAB
    INDEX TC0100-CURRENT_LINE.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TC0100_MARK INPUT.
  DATA: g_TC0100_wa2 like line of ITAB.
    if TC0100-line_sel_mode = 1
    and ITAB-SEL = 'X'.
     loop at ITAB into g_TC0100_wa2
       where SEL = 'X'.
       g_TC0100_wa2-SEL = ''.
       modify ITAB
         from g_TC0100_wa2
         transporting SEL.
     endloop.
  endif.
  MODIFY ITAB
    INDEX TC0100-CURRENT_LINE
    TRANSPORTING SEL.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TC0100_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC0100'
                              'ITAB'
                              'SEL'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME
                          P_MARK_NAME
                 CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: L_OK              TYPE SY-UCOMM,
         L_OFFSET          TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH P_OK FOR P_TC_NAME.
   IF SY-SUBRC <> 0.
     EXIT.
   ENDIF.
   L_OFFSET = STRLEN( P_TC_NAME ) + 1.
   L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE L_OK.
     WHEN 'INSR'.                      "insert row
       PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                         P_TABLE_NAME.
       CLEAR P_OK.

     WHEN 'DELE'.                      "delete row
       PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME.
       CLEAR P_OK.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                             L_OK.
       CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME   .
       CLEAR P_OK.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                           P_TABLE_NAME
                                           P_MARK_NAME .
       CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_insert_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_LINES_NAME       LIKE FELD-NAME.
   DATA L_SELLINE          LIKE SY-STEPL.
   DATA L_LASTLINE         TYPE I.
   DATA L_LINE             TYPE I.
   DATA L_TABLE_NAME       LIKE FELD-NAME.
   FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
   ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
   GET CURSOR LINE L_SELLINE.
   IF SY-SUBRC <> 0.                   " append line to table
     L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
     IF L_SELLINE > <LINES>.
       <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
     ELSE.
       <TC>-TOP_LINE = 1.
     ENDIF.
   ELSE.                               " insert line into table
     L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
     L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
   <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
   SET CURSOR LINE L_LINE.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_delete_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME
                        P_MARK_NAME   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TABLE_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA>.
   FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

   LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     IF <MARK_FIELD> = 'X'.
       DELETE <TABLE> INDEX SYST-TABIX.
       IF SY-SUBRC = 0.
         <TC>-LINES = <TC>-LINES - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                       P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TC_NEW_TOP_LINE     TYPE I.
   DATA L_TC_NAME             LIKE FELD-NAME.
   DATA L_TC_LINES_NAME       LIKE FELD-NAME.
   DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
   IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
     L_TC_NEW_TOP_LINE = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
          EXPORTING
               ENTRY_ACT             = <TC>-TOP_LINE
               ENTRY_FROM            = 1
               ENTRY_TO              = <TC>-LINES
               LAST_PAGE_FULL        = 'X'
               LOOPS                 = <LINES>
               OK_CODE               = P_OK
               OVERLAPPING           = 'X'
          IMPORTING
               ENTRY_NEW             = L_TC_NEW_TOP_LINE
          EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               OTHERS                = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD L_TC_FIELD_NAME
              AREA  L_TC_NAME.

   IF SYST-SUBRC = 0.
     IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA:
    l_flg.

  CLEAR:
    l_flg.

  CASE ok_code .
*   回车
    WHEN 'ENTER'.
**     权限检查
*      PERFORM pur_check.
**     维护检查
      PERFORM f_check.
*     显示数据
      PERFORM f_enter.

*   修改
    WHEN 'CHANGE'.
      CLEAR ok_code.
**     权限检查
*      PERFORM pur_check.
*     锁表
      PERFORM f_lock.

*     显示已初始数据
      PERFORM f_enter.

*     已初始其他月份检查
      PERFORM f_check_init.

*     对账检查
      PERFORM f_check_update.

      flg_chg = 'X'.
      flg_mdf = space.

*   增加
    WHEN 'ADD'.
      IF flg_chg = 'X'.
        PERFORM f_add_item.
      ELSE.
        MESSAGE s888(sabapdocu) WITH  '只允许在修改状态下执行！'.
      ENDIF.


*   删除
    WHEN 'DEL'.
      IF flg_chg = 'X'.
        DELETE itab WHERE sel = 'X'.
      ELSE.
        MESSAGE s888(sabapdocu) WITH  '只允许在修改状态下执行！'.
      ENDIF.



*     显示数据
      PERFORM f_enter.



    WHEN 'SAVE'.
*     权限检查
*      PERFORM pur_check.

      PERFORM f_save.
    WHEN 'ALL'.
      IF flg_chg = 'X'.
        LOOP AT itab.
          itab-sel = 'X'.
          MODIFY itab.
        ENDLOOP.
      ELSE.
        MESSAGE s888(sabapdocu) WITH  '只允许在修改状态下全选数据！'.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_save .
DATA:
    l_flg1 TYPE c,
    l_flg2 TYPE c,
    l_flg3 TYPE c,
    l_flg4 TYPE c,
    l_flg5 TYPE c.

* 重复初始化判断
* 不做变更的情况视为重复初始化
  IF flg_chg = space.
    MESSAGE i888(sabapdocu) WITH  'ERROR:不要重复初始化'.
    EXIT.
*    STOP.
  ENDIF.

* 余额初始化
  IF v_dmbtr2 <> dmbtr2.
    PERFORM f_set_dmbtr2 CHANGING l_flg1.
  ENDIF.
* 银行/企业未清明细初始化
  IF v_itab[] <> itab[].

    PERFORM f_set_detail CHANGING l_flg2
                                  l_flg3
                                  l_flg4
                                  l_flg5.
  ENDIF.

IF l_flg1 = 'X' OR l_flg2 = 'X' OR l_flg3 = 'X' OR l_flg4 = 'X'
  OR l_flg5 = 'X'.
    ROLLBACK WORK.
  ENDIF.

  IF l_flg5 = 'X'.
    MESSAGE i888(sabapdocu) WITH  'ERROR:银行抬头表更新错误'.
    LEAVE TO SCREEN 0.
  ENDIF.

  IF l_flg1 = 'X'.
    MESSAGE i888(sabapdocu) WITH  'ERROR:帐户余额初始化失败'.
    LEAVE TO SCREEN 0.
  ENDIF.
  IF l_flg2 = 'X'.
    MESSAGE i888(sabapdocu) WITH 'ERROR:银行未清明细更新错误'.
    LEAVE TO SCREEN 0.
  ENDIF.
  IF l_flg3 = 'X'.
    MESSAGE i888(sabapdocu) WITH 'ERROR:企业未清明细更新错误'.
    LEAVE TO SCREEN 0.
  ENDIF.
  IF l_flg4 = 'X'.
    MESSAGE i888(sabapdocu) WITH 'ERROR:银行流水明细更新错误'.
    LEAVE TO SCREEN 0.
  ENDIF.

  IF l_flg1 = space AND
     l_flg2 = space AND
     l_flg3 = space AND
     l_flg4 = space.
    COMMIT WORK.
    CALL FUNCTION 'DEQUEUE_ALL'.

   flg_mdf = 'X'.
    flg_chg = space.
    flg_lock = space.
    MESSAGE i888(sabapdocu) WITH '初始化成功'.
  ENDIF.

  PERFORM f_pop_zfir06.


ENDFORM.                    " f_save
*&---------------------------------------------------------------------*
*&      Form  f_set_dmbtr2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FLG1  text
*----------------------------------------------------------------------*
FORM f_set_dmbtr2  CHANGING o_flg.
DATA:
    l_ys(2)     TYPE n,
    l_field(16),
    l_zdimglt0    TYPE zdimglt0,
    l_zdimglt0_t   TYPE zdimglt0,
    lt_zdimglt0   TYPE TABLE OF zdimglt0,
    l_monat TYPE rpmax.
  FIELD-SYMBOLS <fs>.

  l_monat = monat - 1.
  IF l_monat IS INITIAL.
    l_monat = 1.
  ENDIF.
**** 插入余额
  DO l_monat TIMES.
    l_ys = l_ys + 1.
  ENDDO.
  CLEAR l_field.
  CONCATENATE 'L_zdimGLT0-HSL' l_ys INTO l_field.
  ASSIGN (l_field) TO <fs>.

  l_zdimglt0-bukrs = bukrs."公司代码
  l_zdimglt0-hkont = w_hkont."总账科目
  l_zdimglt0-txt50 = txt50."总账科目描述
  l_zdimglt0-zdimuscod = uscod."银行账号
  l_zdimglt0-gjahr = gjahr."年度

  IF dmbtr2 > 0.

    l_zdimglt0-shkzg = 'H'.  "借贷标志
    <fs> = dmbtr2.
    APPEND l_zdimglt0 TO lt_zdimglt0.

    l_zdimglt0-shkzg = 'S'.  "借贷标志
    <fs> = space.
    APPEND l_zdimglt0 TO lt_zdimglt0.
  ELSE.

    l_zdimglt0-shkzg = 'S'.  "借贷标志
    <fs> = -1 * dmbtr2.
    APPEND l_zdimglt0 TO lt_zdimglt0.

    l_zdimglt0-shkzg = 'H'.  "借贷标志
    <fs> = space.
    APPEND l_zdimglt0 TO lt_zdimglt0.
  ENDIF.

  DELETE FROM zdimglt0 WHERE bukrs = bukrs
                       AND hkont = w_hkont
                       AND gjahr = gjahr.
  INSERT zdimglt0 FROM TABLE lt_zdimglt0 ACCEPTING DUPLICATE KEYS.
  IF sy-subrc <> 0.
    o_flg = 'X'.
  ENDIF.
ENDFORM.                    " f_set_dmbtr2
*&---------------------------------------------------------------------*
*&      Form  f_set_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FLG2  text
*      <--P_L_FLG3  text
*      <--P_L_FLG4  text
*      <--P_L_FLG5  text
*----------------------------------------------------------------------*
FORM f_set_detail  CHANGING O_FLG1
                            O_FLG2
                            O_FLG3
                            O_FLG4.
  DATA:
    l_zdimwsbk    TYPE zdimwsbk,
    lt_zdimwsbk   TYPE TABLE OF zdimwsbk,
    l_zdimbsbs     TYPE zdimbsbs,
    lt_zdimbsbs    TYPE TABLE OF zdimbsbs,
    l_zdimwseg    TYPE zdimwseg,
    lt_zdimwseg   TYPE TABLE OF zdimwseg,
    l_monat     TYPE rpmax,
    l_monat_t(2)   TYPE n,
    l_ddate     TYPE d,
    l_zdimwkpf TYPE zdimwkpf.

  l_monat = monat - 1.
  l_monat_t = monat.
  CONCATENATE gjahr l_monat_t '01' INTO l_ddate.
  "获得科目货币 changed by d00194 at 090615
    DATA l_waers TYPE WAERS_SKB1.
    select single waers into l_waers
      from skb1
      where BUKRS = bukrs
        and SAKNR = w_hkont.

      IF sy-SUBRC ne 0.
        MESSAGE E888(sabapdocu) WITH '科目货币获得失败'.
      ENDIF.
    "end of 获得科目货币
  LOOP AT itab.

    IF itab-zdimedate >= l_ddate.
      MESSAGE E888(sabapdocu) WITH '未达明细日期超过初始范围'.
      "STOP.
    ENDIF.

    IF itab-chgty = '1'.                    "企业未达
*     银行对账状态初始化
      MOVE-CORRESPONDING itab TO l_zdimwsbk.
      l_zdimwsbk-zdimcid = '1000000000'.
      l_zdimwsbk-bukrs = bukrs.
      l_zdimwsbk-zdimuscod = uscod.
      l_zdimwsbk-gjahr = gjahr.
      l_zdimwsbk-monat = l_monat.
      l_zdimwsbk-hkont = w_hkont.
      l_zdimwsbk-zdimflag = space.

      l_zdimwsbk-waers = l_waers.
      APPEND l_zdimwsbk TO lt_zdimwsbk.
      CLEAR l_zdimwsbk.

*     银行明细初始化
      MOVE-CORRESPONDING itab TO l_zdimwseg.
      l_zdimwseg-zdimcid = '1000000000'.
      l_zdimwseg-bukrs = bukrs.
      l_zdimwseg-gjahr = gjahr.
      l_zdimwseg-monat = l_monat.
      l_zdimwseg-zdimuscod = uscod.
      l_zdimwseg-uname = sy-uname.
      l_zdimwseg-aedtm = sy-datum.

      l_zdimwseg-waers = l_waers.
      APPEND l_zdimwseg TO  lt_zdimwseg.
      CLEAR l_zdimwseg.


*     银行抬头初始化
      l_zdimwkpf-zdimcid = '1000000000'.
      l_zdimwkpf-bukrs = bukrs.
      l_zdimwkpf-zdimuscod = uscod.
      l_zdimwkpf-zdimedate = itab-zdimedate.
      l_zdimwkpf-erdat = sy-datum.
      l_zdimwkpf-ernam = sy-uname.

      l_zdimwkpf-waers = l_waers.
    ENDIF.

    IF itab-chgty = '0'.      "银行未达
*     企业对账状态初始化
      MOVE-CORRESPONDING itab TO l_zdimbsbs.
      l_zdimbsbs-bukrs = bukrs.
      l_zdimbsbs-hkont = w_hkont.
      l_zdimbsbs-gjahr = gjahr.
      l_zdimbsbs-belnr = itab-zdimcid.
      l_zdimbsbs-buzei = itab-zdimline.
      l_zdimbsbs-monat = l_monat.
      l_zdimbsbs-zdimcid = space.      "银行ID
      l_zdimbsbs-zdimline = space.     "银行明细
      l_zdimbsbs-zdimuscod = uscod.
      l_zdimbsbs-bldat = itab-zdimedate.
      l_zdimbsbs-sgtxt = itab-zdimdtext.
      l_zdimbsbs-ZDIMFLAG = space.

      l_zdimbsbs-waers = l_waers.
      APPEND l_zdimbsbs TO lt_zdimbsbs.
      CLEAR l_zdimbsbs.
    ENDIF.
    CLEAR itab.
  ENDLOOP.

  IF NOT l_zdimwkpf IS INITIAL.
    DELETE FROM zdimwkpf WHERE bukrs = bukrs
                         AND zdimuscod = uscod.
*                         AND GJAHR = GJAHR
*                         AND MONAT = L_MONAT.
    INSERT zdimwkpf FROM l_zdimwkpf." ACCEPTING DUPLICATE KEYS.
    IF sy-subrc <> 0.
      o_flg4 = 'X'.
    ENDIF.
  ENDIF.

  DELETE FROM zdimwseg WHERE bukrs = bukrs
                       AND monat = l_monat
                       AND gjahr = gjahr
                       AND zdimuscod = uscod
                       .
  INSERT zdimwseg FROM TABLE lt_zdimwseg ACCEPTING DUPLICATE KEYS.
  IF sy-subrc <> 0.
    o_flg3 = 'X'.
  ENDIF.

  DELETE FROM zdimwsbk WHERE bukrs = bukrs
*                       AND USCOD = USCOD
                       AND gjahr = gjahr
                       AND monat = l_monat.
  INSERT zdimwsbk FROM TABLE lt_zdimwsbk ACCEPTING DUPLICATE KEYS.
  IF sy-subrc <> 0.
    o_flg1 = 'X'.
  ENDIF.

  DELETE FROM zdimbsbs  WHERE bukrs = bukrs
                       AND hkont = w_hkont
*                       AND USCOD = USCOD
                       AND gjahr = gjahr.
  INSERT zdimbsbs FROM TABLE lt_zdimbsbs ACCEPTING DUPLICATE KEYS.
  IF sy-subrc <> 0.
    o_flg2 = 'X'.
  ENDIF.




ENDFORM.                    " f_set_detail
*&---------------------------------------------------------------------*
*&      Form  f_pop_zfir06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_pop_zfir06 .
  DATA:
    l_answer TYPE c,
    l_monat TYPE rpmax.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '确认对话框'
      text_question         = '建议查看余额调节表，是否查看？'
      text_button_1         = '是'(001)
      text_button_2         = '否'(002)
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
    l_monat = monat - 1.
    SUBMIT ZDIMENSION_FII006
                  WITH p_bukrs = bukrs
                  WITH p_hkont = hkont
                  WITH p_gjahr = gjahr
                  WITH p_monat = l_monat
                  AND RETURN.
  ENDIF.

  IF l_answer = '2'.
   EXIT. "STOP.
  ENDIF.


ENDFORM.                    " f_pop_zfir06
*&---------------------------------------------------------------------*
*&      Module  exit_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.
*   退出
  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " exit_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_enter .
* 新的输入的时候
  IF v_bukrs <> bukrs OR
     v_hkont <> hkont OR
     v_gjahr <> gjahr OR
     v_monat <> monat.
    CLEAR:
      uscod,
      dmbtr1,
      txt50,
      flg_mdf.


****银行账号
    PERFORM f_get_uscod.

****总账科目描述
    PERFORM f_get_txt50.

****企业期初余额
    PERFORM f_get_account.

****已经初始化的银行余额
    PERFORM f_get_dmbtr.

****各种已经初始化的明细
    PERFORM f_get_detail.


    flg_mdf = 'X'.
  ENDIF.

ENDFORM.                    " f_enter
*&---------------------------------------------------------------------*
*&      Form  f_get_dmbtr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_dmbtr .
DATA: l_zdimglt0    TYPE zdimglt0,
        l_bank_hsl  TYPE bapi1028_3-balance,
        l_ys(2)     TYPE n,                     "需汇总的月份数
        l_field(14),                            "表字段名
        l_hkont    TYPE char10,
        l_monat    TYPE monat.

  FIELD-SYMBOLS <fs>.                      "用于动态运算的指针

  l_monat = monat - 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = hkont
    IMPORTING
      output = l_hkont.

  SELECT *
    FROM zdimglt0
    INTO l_zdimglt0
   WHERE bukrs = bukrs
     AND hkont = l_hkont
     AND gjahr = gjahr.
    CLEAR l_ys.
    IF l_zdimglt0-shkzg = 'S'."计算本月借方余额
      DO l_monat TIMES.
        CLEAR l_field.
        l_ys = l_ys + 1.
        CONCATENATE 'L_zdimGLT0-HSL' l_ys INTO l_field.
        ASSIGN (l_field) TO <fs>.
        l_bank_hsl = l_bank_hsl - <fs>.
      ENDDO.
    ENDIF.

    IF l_zdimglt0-shkzg = 'H'."计算本月贷方余额.
      DO l_monat TIMES.
        CLEAR l_field.
        l_ys = l_ys + 1.
        CONCATENATE 'L_zdimGLT0-HSL' l_ys INTO l_field.
        ASSIGN (l_field) TO <fs>.
        l_bank_hsl = l_bank_hsl + <fs>.
      ENDDO.
    ENDIF.
  ENDSELECT.

  dmbtr2 = l_zdimglt0-hslvt + l_bank_hsl.
  v_dmbtr2 = dmbtr2."保留值

ENDFORM.                    " f_get_dmbtr
*&---------------------------------------------------------------------*
*&      Form  f_get_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_detail .
DATA:
    lt_zdimwsbk TYPE TABLE OF zdimwsbk,
    l_zdimwsbk TYPE zdimwsbk.
  DATA:
    lt_zdimbsbs TYPE TABLE OF zdimbsbs,
    l_zdimbsbs TYPE zdimbsbs,
    l_bsis TYPE bsis,
    lt_bsis TYPE TABLE OF bsis,
    l_bsas TYPE bsas,
    lt_bsas TYPE TABLE OF bsas.

  IF v_bukrs <> bukrs OR
     v_hkont <> hkont OR
     v_gjahr <> gjahr OR
     v_monat <> monat.

    CLEAR itab[].
****查找企业未达明细—赋itab
    SELECT *
      FROM zdimwsbk
      INTO CORRESPONDING FIELDS OF TABLE lt_zdimwsbk
     WHERE bukrs = bukrs
       AND gjahr <= gjahr
       AND hkont = w_hkont
       AND monat < monat
       AND ZDIMFLAG = space.
    LOOP AT lt_zdimwsbk INTO l_zdimwsbk.
      itab-zdimline = l_zdimwsbk-zdimline.
      itab-zdimcid = l_zdimwsbk-zdimcid.
      itab-zdimcscod = l_zdimwsbk-zdimcscod.
      itab-uname = l_zdimwsbk-zdimname.
      itab-zdimedate = l_zdimwsbk-zdimedate.
      itab-shkzg = l_zdimwsbk-shkzg.
      itab-chgty = '1'.
      itab-dmbtr = l_zdimwsbk-dmbtr.
      itab-zdimdtext = l_zdimwsbk-zdimdtext.
      APPEND itab.
    ENDLOOP.

****查找银行未达明细
    SELECT *
      FROM zdimbsbs
      INTO CORRESPONDING FIELDS OF TABLE lt_zdimbsbs
     WHERE bukrs = bukrs
       AND gjahr <= gjahr
       AND hkont = w_hkont
       AND monat < monat
       AND ZDIMFLAG = space.
    LOOP AT lt_zdimbsbs INTO l_zdimbsbs.
      itab-zdimline = l_zdimbsbs-buzei.
      itab-zdimcid = l_zdimbsbs-belnr.
      itab-zdimcscod = space.
      itab-uname = space.
      itab-zdimedate = l_zdimbsbs-bldat.
      itab-shkzg = l_zdimbsbs-shkzg.
      itab-chgty = '0'.
      itab-dmbtr = l_zdimbsbs-dmbtr.
      itab-zdimdtext = l_zdimbsbs-sgtxt.
      APPEND itab.
    ENDLOOP.
*   保留画面输入
    v_bukrs = bukrs.
    v_hkont = hkont.
    v_gjahr = gjahr.
    v_monat = monat.
    v_itab[] = itab[].
  ENDIF.

ENDFORM.                    " f_get_detail
*&---------------------------------------------------------------------*
*&      Form  pur_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pur_check .
DATA : msg TYPE string,
         p_hkont TYPE zdimbuhk-hkont.
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
  WHERE bukrs = bukrs
  .
  IF LINES( i_bukrs ) = 0.
    MESSAGE '请重新选择条件，不存在公司条件' TYPE 'I'.
    EXIT."STOP.
  ELSE.
    LOOP AT i_bukrs.
      AUTHORITY-CHECK OBJECT 'Z_AUTH_CHK'
               ID 'BUKRS' FIELD i_bukrs-bukrs
*               ID 'WERKS' FIELD i_bukrs-werks
*               ID 'VKORG' FIELD '*'
*               ID 'VTWEG' FIELD '*'
*               ID 'EKORG' FIELD '*'
               .
      g_str = i_bukrs-bukrs.
      IF sy-subrc NE 0.
        CONCATENATE  '没有' g_str '公司的权限' INTO g_str.
        "MESSAGE g_str TYPE 'E'.
*      "STOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
*-------------------------------------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = hkont
    IMPORTING
      output = p_hkont.
  AUTHORITY-CHECK OBJECT 'Z_FI_BANK' ID 'BUKRS' FIELD bukrs
                                     ID 'SAKNR' FIELD p_hkont.
  IF sy-subrc <> 0.
    CONCATENATE '你没有公司代码' bukrs
                '及其对应科目' hkont '的权限！' INTO msg.
    MESSAGE i888(sabapdocu) WITH msg.
*    STOP.
  ENDIF.

ENDFORM.                    " pur_check
*&---------------------------------------------------------------------*
*&      Form  f_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check .

  DATA:
    l_zdimbuhk TYPE zdimbuhk,
    l_hkont TYPE char10.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = hkont
    IMPORTING
      output = l_hkont.

  SELECT SINGLE *
    FROM zdimbuhk
    INTO l_zdimbuhk
   WHERE bukrs = bukrs
     AND hkont = l_hkont
     AND zdimflag2 = 'X'.
  IF sy-subrc <> 0.
    MESSAGE e888(sabapdocu) WITH  'ERROR:科目与账号没有维护'.
    "STOP.
    "LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " f_check
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'GUISTAT'.
  SET TITLEBAR 'GUITIT'.




ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  get_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_lines OUTPUT.
DATA:
    line TYPE i.

  DESCRIBE TABLE itab LINES line.
* 控制tc行数跟内表一样
  "G_TC0100_LINES = line.
  tc0100-lines = line.

* 设定输出可否输入的范围设定
  IF flg_chg = 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'DMBTR2'.

        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = 'DMBTR2'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.


  ENDIF.



ENDMODULE.                 " get_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_itab  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_itab INPUT.
**** 按屏幕输入值更改内表
  MODIFY itab INDEX tc0100-current_line.

ENDMODULE.                 " modify_itab  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_add_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_add_item .
DATA:
    l_line TYPE i,
    l_zdimline TYPE zdimwseg-zdimline.

  IF flg_chg = 'X'.
*   行号设定
    SORT itab BY zdimline.
    DESCRIBE TABLE itab LINES l_line.
    READ TABLE itab INDEX l_line.
*   保留目前最大行号
    l_zdimline = itab-zdimline.
    CLEAR itab.
    itab-zdimline = l_zdimline + 1.
    APPEND itab.
  ENDIF.

ENDFORM.                    " f_add_item
*&---------------------------------------------------------------------*
*&      Form  f_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lock .
*  IF flg_lock = space.
*
*    PERFORM f_lock_zdimglt0.
*    PERFORM f_lock_zdimwkpf.
*    PERFORM f_lock_zdimwseg.
**
*    PERFORM f_lock_zdimbsbs.
*    PERFORM f_lock_zdimwsbk.
*
*  ENDIF.

*  flg_lock = 'X'.

ENDFORM.                    " f_lock
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimglt0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lock_zdimglt0 .
  CALL FUNCTION 'ENQUEUE_EZDIMGLT0'
    EXPORTING
      mode_ZDIMGLT0    = 'X'
      mandt          = sy-mandt
      bukrs          = bukrs
      hkont          = w_hkont
      ZDIMUSCOD          = uscod
      gjahr          = gjahr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    MESSAGE i888(sabapdocu) WITH  'ERROR:银行期末余额锁表失败'.
    LEAVE PROGRAM.
  ENDIF.


ENDFORM.                    " f_lock_zdimglt0
*&---------------------------------------------------------------------*
*&      Module  SET_D0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_D0100 OUTPUT.
*初始化之前控制可输入范围
  IF flg_mdf IS INITIAL.
    LOOP AT SCREEN .
      IF tc0100-current_line > tc0100-lines.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
*  初始化完成之后不可输入的范围

    LOOP AT SCREEN .

      IF screen-name = 'BUKRS' OR
         screen-name = 'HKONT' OR
         screen-name = 'GJAHR' OR
         screen-name = 'MONAT'.

        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " SET_D0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SHOWF4_SHKZG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SHOWF4_SHKZG INPUT.

  REFRESH values_tab1.
  CLEAR values_tab1.

  values_tab1-shkzg = 'S'.
  values_tab1-txt = '借方'.
  APPEND values_tab1.CLEAR values_tab1.
  values_tab1-shkzg = 'H'.
  values_tab1-txt = '贷方'.
  APPEND values_tab1.CLEAR values_tab1.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'SHKZG'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ITAB-TXT'  "屏幕上的名称
      value_org   = 'S'
    TABLES
      value_tab   = values_tab1.

ENDMODULE.                 " SHOWF4_SHKZG  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHOWF4_CHGTY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SHOWF4_CHGTY INPUT.
REFRESH values_tab2.
  CLEAR values_tab2.

  values_tab2-chgty = '0'.
  values_tab2-txt = '银行未达'.
  APPEND values_tab2.CLEAR values_tab2.
  values_tab2-chgty = '1'.
  values_tab2-txt = '企业未达'.
  APPEND values_tab2.CLEAR values_tab2.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CHGTY'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ITAB-CHGTY'  "屏幕上的名称
      value_org   = 'S'
    TABLES
      value_tab   = values_tab2.

ENDMODULE.                 " SHOWF4_CHGTY  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_get_account
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_account .
   DATA:
    l_account_balance TYPE bapi1028_3-balance,
    l_account         TYPE TABLE OF zbapi_acc,
    l_temp            TYPE zbapi_acc,
    l_monat           TYPE rpmax.
  CLEAR:
    l_account_balance,
    l_account,
    l_temp,
    l_monat.

  l_temp-gl_account = w_hkont.
  APPEND l_temp TO l_account.

  l_monat = monat - 1.

  CALL FUNCTION 'ZDIMENSION_FED_GET_SUB_BALANCE'
    EXPORTING
      rclnt           = sy-mandt
      bukrs           = bukrs
      ryear           = gjahr
      rpmax           = l_monat
    IMPORTING
      account_balance = l_account_balance
    TABLES
      account         = l_account.

  IF sy-subrc = 0.
    dmbtr1 = l_account_balance.
  ENDIF.

ENDFORM.                    " f_get_account

*&---------------------------------------------------------------------*
*&      Form  f_get_uscod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_uscod .
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = hkont
    IMPORTING
      output = w_hkont.

  SELECT SINGLE zdimuscod
    INTO uscod
    FROM zdimbuhk
   WHERE bukrs = bukrs
     AND hkont = w_hkont.

ENDFORM.                    " f_get_uscod
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimwkpf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lock_zdimwkpf .
  DATA:
    l_monat TYPE zdimwsbk-monat.
  l_monat = monat.
  CALL FUNCTION 'ENQUEUE_EZDIMWKPF'
    EXPORTING
      mode_zdimwkpf    = 'X'
      mandt          = sy-mandt
*      bukrs          = bukrs
*      zdimuscod          = uscod
      "gjahr          = gjahr
      "monat          = l_monat
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE i888(sabapdocu) WITH  'ERROR:银行流水账状态表锁表失败'.
    LEAVE PROGRAM.
  ENDIF.


ENDFORM.                    " f_lock_zdimwkpf
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimwseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lock_zdimwseg .
  DATA:
    l_monat TYPE zdimwseg-monat.

  l_monat = monat.
  CALL FUNCTION 'ENQUEUE_EZDIMWSEG'
    EXPORTING
      mode_zdimwseg    = 'X'
      mandt          = sy-mandt
*      bukrs          = bukrs
*      gjahr          = gjahr
*      monat          = l_monat
*      zdimuscod          = uscod
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE i888(sabapdocu) WITH  'ERROR:银行流水账明细表锁表失败'.
    LEAVE PROGRAM.
  ENDIF.


ENDFORM.                    " f_lock_zdimwseg
*&---------------------------------------------------------------------*
*&      Module  check_hkont_authority  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_hkont_authority INPUT.
*     权限检查
      PERFORM pur_check.
ENDMODULE.                 " check_hkont_authority  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_BUKRS_authority  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_BUKRS_authority INPUT.
  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
*     维护检查
      PERFORM f_check.
ENDMODULE.                 " check_BUKRS_authority  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_get_txt50
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_txt50 .
SELECT SINGLE txt50  "总账科目描述
    FROM zdimbuhk
    INTO txt50
   WHERE bukrs = bukrs
     AND hkont = w_hkont.

ENDFORM.                    " f_get_txt50
*&---------------------------------------------------------------------*
*&      Form  f_check_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_update .
DATA:
    lt_zdimwsbk TYPE TABLE OF zdimwsbk,
    l_zdimwsbk TYPE zdimwsbk.

  SELECT * UP TO 1 ROWS
    FROM zdimwsbk
    INTO CORRESPONDING FIELDS OF l_zdimwsbk
   WHERE bukrs = bukrs
     AND zdimuscod = uscod
     AND zdimflag = 'X'.
  ENDSELECT.

  IF sy-subrc = 0.
    MESSAGE i888(sabapdocu) WITH
                            'ERROR:请先将单据取消对帐'.
*    STOP.
  ENDIF.
ENDFORM.                    " f_check_update
*&---------------------------------------------------------------------*
*&      Module  SHOWF4_HKONT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SHOWF4_HKONT INPUT.
  PERFORM frm_help_hkont USING hkont.
ENDMODULE.                 " SHOWF4_HKONT  INPUT
*&---------------------------------------------------------------------*
*&      Form  frm_help_hkont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_help_hkont USING    P_HKONT like bseg-hkont.
  TABLES T001.
  data: begin of it_hkont occurs 0,
        BUKRS like ZDIMBUHK-bukrs,
        HKONT like ZDiMBUHK-hkont,
        TXT50 like ZDiMBUHK-txt50,
        ZDIMUSCOD like ZDiMBUHK-zdimuscod,
        uscodTXT  like ZDiMBUHK-txt50,
      end of it_hkont.
data: i_bukrs like bseg-bukrs.
  DATA: BEGIN OF DYNPFIELDS OCCURS 5.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF DYNPFIELDS.
  data: RETURN_TAB LIKE  table of DDSHRETVAL with header line.
  FIELD-SYMBOLS: <fs_HKONT> like it_HKONT.
  clear: it_HKONT[],
         it_HKONT.
  MOVE 'BUKRS' TO DYNPFIELDS-FIELDNAME.
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
    READ TABLE DYNPFIELDS WITH KEY 'BUKRS'.
    IF SY-SUBRC = 0.
      i_bukrs = DYNPFIELDS-FIELDVALUE.
    endif.
  endif.
  select * into CORRESPONDING FIELDS OF TABLE it_HKONT
    from ZDiMBUHK
    where BUKRS = i_bukrs AND zdimflag2 = 'X'.
  select single *
    from t001
    where BUKRS = i_bukrs.
  loop at it_HKONT ASSIGNING <fs_HKONT>.
    select single TXT50 into <fs_HKONT>-TXT50
      from SKAt
      where SPRAS = sy-langu
        and KTOPL = t001-KTOPL
        and SAKNR = <fs_HKONT>-hkont.
    select single TXT50 into <fs_HKONT>-uscodTXT
      from zdimbno
      where zdimuscod = <fs_HKONT>-zdimuscod.
  endloop.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'HKONT'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'HKONT'  "屏幕上的名称
      value_org       = 'S'
    TABLES
      value_tab       = it_HKONT
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
    P_HKONT = RETURN_TAB-fieldval.
  ENDIF.
ENDFORM.                    " frm_help_hkont
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimbsbs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lock_zdimbsbs .
  CALL FUNCTION 'ENQUEUE_EZDIMBSBS'
    EXPORTING
      mode_zdimbsbs     = 'X'
      mandt          = sy-mandt
      bukrs          = bukrs
      hkont          = w_hkont
      gjahr          = gjahr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE i888(sabapdocu) WITH  'ERROR:会计凭证对帐状态表锁表失败'.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " f_lock_zdimbsbs
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimwsbk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lock_zdimwsbk .
DATA:
    l_monat TYPE zdimwsbk-monat.
  l_monat = monat.
  CALL FUNCTION 'ENQUEUE_EZDIMWSBK'
    EXPORTING
      mode_zdimwsbk    = 'X'
      mandt          = sy-mandt
      bukrs          = bukrs
      zdimuscod          = uscod
      gjahr          = gjahr
      monat          = l_monat
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE i888(sabapdocu) WITH  'ERROR:银行流水账状态表锁表失败'.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " f_lock_zdimwsbk
*&---------------------------------------------------------------------*
*&      Form  f_check_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_init .
DATA:
    l_zdimwseg TYPE zdimwseg,
    l_text TYPE string,
    l_monat TYPE rpmax.

  l_monat = monat - 1.
**** 已初始其他月份检查
  SELECT * UP TO 1 ROWS
    FROM zdimwseg
    INTO CORRESPONDING FIELDS OF l_zdimwseg
   WHERE bukrs = bukrs
     AND gjahr = gjahr
     AND zdimuscod = uscod
     AND monat <> l_monat.
  ENDSELECT.

  IF sy-subrc = 0.

    CONCATENATE '只能初始一个月的期初值，请先将'
                l_zdimwseg-monat
                '月账单撤销!'
           INTO l_text.
    MESSAGE e888(sabapdocu) WITH  l_text.
    STOP.

  ENDIF.
ENDFORM.                    " f_check_init
