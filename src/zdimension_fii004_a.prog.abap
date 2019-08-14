*&---------------------------------------------------------------------*
*& Report  ZDIMENSION_FII004_A
*& Description: 余额初始化程序                                         *
*& Create By  : D00194                                                *
*& Create Date: 2009/05/11
*& Project Name          :  达美银行对账项目

*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report  zdimension_fii004_a.
tables t001.

data:
  ok_code type sy-ucomm.

*INTERNAL TABLE FOR DATA------------------------------------------------*
data:
*TC用内表
  begin of itab occurs 0,
    sel,                              "选中标志
    zdimline  type zdimwseg-zdimline,   "计数器
    zdimcid   type zdimwseg-zdimcid,    "银行账单ID
    zdimcscod type zdimwseg-zdimcscod,     "行项目的参考码(对方ID)
    uname     type zdimwseg-uname,     "用户名
    zdimedate type zdimwseg-zdimedate,     "交易日期/凭证日期
    shkzg     type zdimwseg-shkzg,
    chgty ,                      "未达类型
    dmbtr     type zdimwseg-dmbtr,     "金额
    waers     type zdimwseg-waers,                          "币别20190617
    zdimdtext type zdimwseg-zdimdtext,     "描述
  end of itab,
*保留用tc表
  begin of v_itab occurs 0,
    sel,
    zdimline  type zdimwseg-zdimline,
    zdimcid   type zdimwseg-zdimcid,
    zdimcscod type zdimwseg-zdimcscod,
    uname     type zdimwseg-uname,
    zdimedate type zdimwseg-zdimedate,
    shkzg     type zdimwseg-shkzg,
    chgty ,
    dmbtr     type zdimwseg-dmbtr,
    waers     like zdimwseg-waers,                          "币别20190617
    zdimdtext type zdimwseg-zdimdtext,
  end of v_itab,
*借贷标识帮助内表
  begin of values_tab1 occurs 0 ,"
    shkzg like bsis-shkzg , "借贷标识
    txt   type zdimdtext, "zshktxt ,"描述
  end of values_tab1,
*调节类型帮助内表
  begin of values_tab2 occurs 0 ,"
    chgty type char1 , "类型
    txt   type zdimdtext, "zchtxt,"描述
  end of values_tab2.

.

*END OF INTERNAL TABLE FOR DATA------------------------------------------*

*屏幕用变量
data:
  flg_mdf,                              "画面可更改标记
  flg_chg,                              "更改按钮
  flg_lock,                             "锁标记
  v_bukrs    type bukrs,                   "保留用
  v_hkont    type hkont,                   "保留用
  v_gjahr    type gjahr,                   "保留用
  v_monat    type rpmax,                   "保留用
  v_uscod    type zdimbuhk-zdimuscod,

  bukrs      type zdimbuhk-bukrs,                     "屏幕用
  hkont      type zdimbuhk-hkont,
  gjahr      type gjahr,
  monat      type rpmax,
  uscod      type zdimbuhk-zdimuscod,              "银行账号
  G_WAERS  TYPE WAERS,                    "币种
  rldnr      type fagl_rldnr,

  txt50      type zdimbuhk-txt50,              "科目描述
  dmbtr2     type zdimwsbk-dmbtr,             "银行余额
  dmbtr1(12) type p decimals 2,         "企业余额
  v_dmbtr2   type zdimwsbk-dmbtr,           "保留画面银行余额

  v_rldnr    like rldnr,

  w_hkont    type char10.                  "格式转化用会计科目

call screen 0100.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC0100' ITSELF
controls: tc0100 type tableview using screen 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'TC0100'
data:     g_tc0100_lines  like sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
module tc0100_change_tc_attr output.
  describe table itab lines tc0100-lines.
endmodule.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
module tc0100_get_lines output.
  g_tc0100_lines = sy-loopc.
endmodule.

*&SPWIZARD: INPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
module tc0100_modify input.
  modify itab
    index tc0100-current_line.
endmodule.

*&SPWIZARD: INPUT MODUL FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
module tc0100_mark input.
  data: g_tc0100_wa2 like line of itab.
  if tc0100-line_sel_mode = 1
  and itab-sel = 'X'.
    loop at itab into g_tc0100_wa2
      where sel = 'X'.
      g_tc0100_wa2-sel = ''.
      modify itab
        from g_tc0100_wa2
        transporting sel.
    endloop.
  endif.
  modify itab
    index tc0100-current_line
    transporting sel.
endmodule.

*&SPWIZARD: INPUT MODULE FOR TC 'TC0100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
module tc0100_user_command input.
  ok_code = sy-ucomm.
  perform user_ok_tc using    'TC0100'
                              'ITAB'
                              'SEL'
                     changing ok_code.
  sy-ucomm = ok_code.
endmodule.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
form user_ok_tc using    p_tc_name type dynfnam
                         p_table_name
                         p_mark_name
                changing p_ok      like sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data: l_ok     type sy-ucomm,
        l_offset type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  search p_ok for p_tc_name.
  if sy-subrc <> 0.
    exit.
  endif.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  case l_ok.
    when 'INSR'.                      "insert row
      perform fcode_insert_row using    p_tc_name
                                        p_table_name.
      clear p_ok.

    when 'DELE'.                      "delete row
      perform fcode_delete_row using    p_tc_name
                                        p_table_name
                                        p_mark_name.
      clear p_ok.

    when 'P--' or                     "top of list
         'P-'  or                     "previous page
         'P+'  or                     "next page
         'P++'.                       "bottom of list
      perform compute_scrolling_in_tc using p_tc_name
                                            l_ok.
      clear p_ok.
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
    when 'MARK'.                      "mark all filled lines
      perform fcode_tc_mark_lines using p_tc_name
                                        p_table_name
                                        p_mark_name   .
      clear p_ok.

    when 'DMRK'.                      "demark all filled lines
      perform fcode_tc_demark_lines using p_tc_name
                                          p_table_name
                                          p_mark_name .
      clear p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  endcase.

endform.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
form fcode_insert_row
              using    p_tc_name           type dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_lines_name       like feld-name.
  data l_selline          like sy-stepl.
  data l_lastline         type i.
  data l_line             type i.
  data l_table_name       like feld-name.
  field-symbols <tc>                 type cxtab_control.
  field-symbols <table>              type standard table.
  field-symbols <lines>              type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  concatenate 'G_' p_tc_name '_LINES' into l_lines_name.
  assign (l_lines_name) to <lines>.

*&SPWIZARD: get current line                                           *
  get cursor line l_selline.
  if sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    if l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    else.
      <tc>-top_line = 1.
    endif.
  else.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  endif.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  insert initial line into <table> index l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  set cursor line l_line.

endform.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
form fcode_delete_row
              using    p_tc_name           type dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  describe table <table> lines <tc>-lines.

  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    if <mark_field> = 'X'.
      delete <table> index syst-tabix.
      if sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      endif.
    endif.
  endloop.

endform.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
form compute_scrolling_in_tc using    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_tc_new_top_line     type i.
  data l_tc_name             like feld-name.
  data l_tc_lines_name       like feld-name.
  data l_tc_field_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <lines>      type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  concatenate 'G_' p_tc_name '_LINES' into l_tc_lines_name.
  assign (l_tc_lines_name) to <lines>.


*&SPWIZARD: is no line filled?                                         *
  if <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  else.
*&SPWIZARD: no, ...                                                    *
    call function 'SCROLLING_IN_TABLE'
      exporting
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      importing
        entry_new      = l_tc_new_top_line
      exceptions
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        others         = 0.
  endif.

*&SPWIZARD: get actual tc and column                                   *
  get cursor field l_tc_field_name
             area  l_tc_name.

  if syst-subrc = 0.
    if l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      set cursor field l_tc_field_name line 1.
    endif.
  endif.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


endform.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_mark_lines using p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    <mark_field> = 'X'.
  endloop.
endform.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_demark_lines using p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    <mark_field> = space.
  endloop.
endform.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  data:
    l_flg.

  clear:
    l_flg.

  case ok_code .
*   回车
    when 'ENTER'.
**     权限检查
*      PERFORM pur_check.
**     维护检查
      perform f_check.
*     显示数据
      perform f_enter.


*   修改
    when 'CHANGE'.
      clear ok_code.
**     权限检查
*      PERFORM pur_check.
*     锁表
      perform f_lock.

*     显示已初始数据
      perform f_enter.

*     已初始其他月份检查
      perform f_check_init.

*     对账检查
      perform f_check_update.

      flg_chg = 'X'.
      flg_mdf = space.

*   增加
    when 'ADD'.
      if flg_chg = 'X'.
        perform f_add_item.
      else.
        message s888(sabapdocu) with  '只允许在修改状态下执行！'.
      endif.

*   删除
    when 'DEL'.
      if flg_chg = 'X'.
        delete itab where sel = 'X'.
      else.
        message s888(sabapdocu) with  '只允许在修改状态下执行！'.
      endif.



*     显示数据
      perform f_enter.



    when 'SAVE'.
*     权限检查
*      PERFORM pur_check.

      perform f_save.
    when 'ALL'.
      if flg_chg = 'X'.
        loop at itab.
          itab-sel = 'X'.
          modify itab.
        endloop.
      else.
        message s888(sabapdocu) with  '只允许在修改状态下全选数据！'.
      endif.

    when others.

  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_save .
  data:
    l_flg1 type c,
    l_flg2 type c,
    l_flg3 type c,
    l_flg4 type c,
    l_flg5 type c.

* 重复初始化判断
* 不做变更的情况视为重复初始化
  if flg_chg = space.
    message i888(sabapdocu) with  'ERROR:不要重复初始化'.
    exit.
*    STOP.
  endif.

* 余额初始化
  if v_dmbtr2 <> dmbtr2.
    perform f_set_dmbtr2 changing l_flg1.
  endif.
* 银行/企业未清明细初始化
  if v_itab[] <> itab[].

    perform f_set_detail changing l_flg2
                                  l_flg3
                                  l_flg4
                                  l_flg5.
  endif.

  if l_flg1 = 'X' or l_flg2 = 'X' or l_flg3 = 'X' or l_flg4 = 'X'
    or l_flg5 = 'X'.
    rollback work.
  endif.

  if l_flg5 = 'X'.
    message i888(sabapdocu) with  'ERROR:银行抬头表更新错误'.
    leave to screen 0.
  endif.

  if l_flg1 = 'X'.
    message i888(sabapdocu) with  'ERROR:帐户余额初始化失败'.
    leave to screen 0.
  endif.
  if l_flg2 = 'X'.
    message i888(sabapdocu) with 'ERROR:银行未清明细更新错误'.
    leave to screen 0.
  endif.
  if l_flg3 = 'X'.
    message i888(sabapdocu) with 'ERROR:企业未清明细更新错误'.
    leave to screen 0.
  endif.
  if l_flg4 = 'X'.
    message i888(sabapdocu) with 'ERROR:银行流水明细更新错误'.
    leave to screen 0.
  endif.

  if l_flg1 = space and
     l_flg2 = space and
     l_flg3 = space and
     l_flg4 = space.
    commit work.
    call function 'DEQUEUE_ALL'.

    flg_mdf = 'X'.
    flg_chg = space.
    flg_lock = space.
    message i888(sabapdocu) with '初始化成功'.
  endif.

  perform f_pop_zfir06.


endform.                    " f_save
*&---------------------------------------------------------------------*
*&      Form  f_set_dmbtr2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FLG1  text
*----------------------------------------------------------------------*
form f_set_dmbtr2  changing o_flg.
  data:
    l_ys(2)      type n,
    l_field(16),
    l_zdimglt0   type zdimglt0,
    l_zdimglt0_t type zdimglt0,
    lt_zdimglt0  type table of zdimglt0,
    l_monat      type rpmax.
  field-symbols <fs>.

  l_monat = monat - 1.

  if l_monat is initial.
    l_monat = 1.
  endif.
**** 插入余额
  do l_monat times.
    l_ys = l_ys + 1.
  enddo.
  clear l_field.
  concatenate 'L_zdimGLT0-HSL' l_ys into l_field.
  assign (l_field) to <fs>.

  l_zdimglt0-bukrs = bukrs."公司代码
  l_zdimglt0-hkont = w_hkont."总账科目
  "l_zdimglt0-txt50 = txt50."总账科目描述
  l_zdimglt0-zdimuscod = uscod."银行账号
  l_zdimglt0-gjahr = gjahr."年度

  if dmbtr2 > 0.

    l_zdimglt0-shkzg = 'H'.  "借贷标志
    <fs> = dmbtr2.
    append l_zdimglt0 to lt_zdimglt0.

    l_zdimglt0-shkzg = 'S'.  "借贷标志
    <fs> = space.
    append l_zdimglt0 to lt_zdimglt0.
  else.

    l_zdimglt0-shkzg = 'S'.  "借贷标志
    <fs> = -1 * dmbtr2.
    append l_zdimglt0 to lt_zdimglt0.

    l_zdimglt0-shkzg = 'H'.  "借贷标志
    <fs> = space.
    append l_zdimglt0 to lt_zdimglt0.
  endif.
  if sy-saprl < 700.
  else."IF SY-SAPRL < 700.
  endif."IF SY-SAPRL < 700.
  delete from zdimglt0 where bukrs = bukrs
                       and hkont = w_hkont
                       and gjahr = gjahr
                       and zdimuscod = uscod.
  insert zdimglt0 from table lt_zdimglt0 accepting duplicate keys.
  if sy-subrc <> 0.
    o_flg = 'X'.
  endif.
endform.                    " f_set_dmbtr2
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
form f_set_detail  changing o_flg1
                            o_flg2
                            o_flg3
                            o_flg4.
  data:
    l_zdimwsbk   type zdimwsbk,
    lt_zdimwsbk  type table of zdimwsbk,
    l_zdimbsbs   type zdimbsbs,
    lt_zdimbsbs  type table of zdimbsbs,
    l_zdimwseg   type zdimwseg,
    lt_zdimwseg  type table of zdimwseg,
    l_monat      type rpmax,
    l_monat_t(2) type n,
    l_ddate      type d,
    l_zdimwkpf   type zdimwkpf.

  l_monat = monat - 1.
  l_monat_t = monat.
  concatenate gjahr l_monat_t '01' into l_ddate.

  "获得科目货币 changed by d00194 at 090615
  data l_waers type waers_skb1.
  select single waers into l_waers
    from skb1
    where bukrs = bukrs
      and saknr = w_hkont.

  if sy-subrc ne 0.
    message e888(sabapdocu) with '科目货币获取失败'.
  endif.
  "end of 获得科目货币
  loop at itab.

    if itab-zdimedate >= l_ddate.
      message e888(sabapdocu) with '未达明细日期超过初始范围'.
      "STOP.
    endif.



    if itab-chgty = '1'.                    "企业未达
*     银行对账状态初始化
      move-corresponding itab to l_zdimwsbk.
      l_zdimwsbk-zdimcid = '1000000000'.
      l_zdimwsbk-bukrs = bukrs.
      l_zdimwsbk-zdimuscod = uscod.
      l_zdimwsbk-gjahr = gjahr.
      l_zdimwsbk-monat = l_monat.
      l_zdimwsbk-hkont = w_hkont.
      l_zdimwsbk-zdimflag = space.

      l_zdimwsbk-waers = l_waers.
      append l_zdimwsbk to lt_zdimwsbk.
      clear l_zdimwsbk.

*     银行明细初始化
      move-corresponding itab to l_zdimwseg.
      l_zdimwseg-zdimcid = '1000000000'.
      l_zdimwseg-bukrs = bukrs.
      l_zdimwseg-gjahr = gjahr.
      l_zdimwseg-monat = l_monat.
      l_zdimwseg-zdimuscod = uscod.
      l_zdimwseg-uname = sy-uname.
      l_zdimwseg-aedtm = sy-datum.

      l_zdimwseg-waers = l_waers.
      append l_zdimwseg to  lt_zdimwseg.
      clear l_zdimwseg.


*     银行抬头初始化
      l_zdimwkpf-zdimcid = '1000000000'.
      l_zdimwkpf-bukrs = bukrs.
      l_zdimwkpf-zdimuscod = uscod.
      l_zdimwkpf-zdimedate = itab-zdimedate.
      l_zdimwkpf-erdat = sy-datum.
      l_zdimwkpf-ernam = sy-uname.

      l_zdimwkpf-waers = l_waers.
    endif.

    if itab-chgty = '0'.      "银行未达
*     企业对账状态初始化
      move-corresponding itab to l_zdimbsbs.
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
      l_zdimbsbs-zdimflag = space.

      l_zdimbsbs-waers = l_waers.
      append l_zdimbsbs to lt_zdimbsbs.
      clear l_zdimbsbs.
    endif.
    clear itab.
  endloop.

  if not l_zdimwkpf is initial.
    delete from zdimwkpf where bukrs = bukrs
                         and zdimuscod = uscod.
*                         AND GJAHR = GJAHR
*                         AND MONAT = L_MONAT.
    insert zdimwkpf from l_zdimwkpf." ACCEPTING DUPLICATE KEYS.
    if sy-subrc <> 0.
      o_flg4 = 'X'.
    endif.
  endif.

  delete from zdimwseg where bukrs = bukrs
                       and monat = l_monat
                       and gjahr = gjahr
                       and zdimuscod = uscod
                       .
  insert zdimwseg from table lt_zdimwseg accepting duplicate keys.
  if sy-subrc <> 0.
    o_flg3 = 'X'.
  endif.

  delete from zdimwsbk where bukrs = bukrs
                       and zdimuscod = uscod
                       and gjahr = gjahr
                       and monat = l_monat.
  insert zdimwsbk from table lt_zdimwsbk accepting duplicate keys.
  if sy-subrc <> 0.
    o_flg1 = 'X'.
  endif.

  delete from zdimbsbs  where bukrs = bukrs
                       and hkont = w_hkont
                       and zdimuscod = uscod
                       and gjahr = gjahr.
  insert zdimbsbs from table lt_zdimbsbs accepting duplicate keys.
  if sy-subrc <> 0.
    o_flg2 = 'X'.
  endif.




endform.                    " f_set_detail
*&---------------------------------------------------------------------*
*&      Form  f_pop_zfir06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_pop_zfir06 .
  data:
    l_answer type c,
    l_monat  type rpmax.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = '确认对话框'
      text_question         = '建议查看余额调节表，是否查看？'
      text_button_1         = '是'(001)
      text_button_2         = '否'(002)
      default_button        = '1'
      display_cancel_button = space
      start_column          = 25
      start_row             = 6
    importing
      answer                = l_answer
    exceptions
      text_not_found        = 1
      others                = 2.
  if sy-subrc <> 0.
    leave to screen 0.
  endif.

  if l_answer = '1'.
*   调用余额调节表程序
    l_monat = monat - 1.
    submit zdimension_fii006_a
                  with p_bukrs = bukrs
                  with p_hkont = hkont
                  with p_gjahr = gjahr
                  with p_monat = l_monat
                  with p_uscod = uscod
                  "WITH p_rldnr = rldnr
                  and return.
  endif.

  if l_answer = '2'.
    exit. "STOP.
  endif.


endform.                    " f_pop_zfir06
*&---------------------------------------------------------------------*
*&      Module  exit_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit_command_0100 input.
*   退出
  case ok_code.
    when 'EXIT' or 'BACK' or 'CANC'.
      leave program.
  endcase.

endmodule.                 " exit_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_enter .
* 新的输入的时候
  if v_bukrs <> bukrs or
     v_hkont <> hkont or
     v_gjahr <> gjahr or
     v_monat <> monat or
     v_uscod <> uscod
    or v_rldnr <> rldnr""
    .
    clear:
      ""uscod,
      dmbtr1,
      txt50,
      flg_mdf.

****分类账
    perform f_get_ldgrp.

****银行账号
    perform f_get_uscod.

****总账科目描述
    perform f_get_txt50.

****企业期初余额
    perform f_get_account.

****已经初始化的银行余额
    perform f_get_dmbtr.

****各种已经初始化的明细
    perform f_get_detail.


    flg_mdf = 'X'.

  endif.

endform.                    " f_enter
*&---------------------------------------------------------------------*
*&      Form  f_get_dmbtr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_dmbtr .
  data: l_zdimglt0  type zdimglt0,
        l_bank_hsl  type bapi1028_3-balance,
        l_ys(2)     type n,                     "需汇总的月份数
        l_field(14),                            "表字段名
        l_hkont     type char10,
        l_monat     type monat.

  field-symbols <fs>.                      "用于动态运算的指针

  l_monat = monat - 1.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = hkont
    importing
      output = l_hkont.

  select *
    from zdimglt0
    into l_zdimglt0
   where bukrs = bukrs
     and hkont = l_hkont
     and zdimuscod = uscod
     and gjahr = gjahr.
    clear l_ys.
    if l_zdimglt0-shkzg = 'S'."计算本月借方余额
      do l_monat times.
        clear l_field.
        l_ys = l_ys + 1.
        concatenate 'L_zdimGLT0-HSL' l_ys into l_field.
        assign (l_field) to <fs>.
        l_bank_hsl = l_bank_hsl - <fs>.
      enddo.
    endif.

    if l_zdimglt0-shkzg = 'H'."计算本月贷方余额.
      do l_monat times.
        clear l_field.
        l_ys = l_ys + 1.
        concatenate 'L_zdimGLT0-HSL' l_ys into l_field.
        assign (l_field) to <fs>.
        l_bank_hsl = l_bank_hsl + <fs>.
      enddo.
    endif.
  endselect.

  dmbtr2 = l_zdimglt0-hslvt + l_bank_hsl.
  v_dmbtr2 = dmbtr2."保留值

endform.                    " f_get_dmbtr
*&---------------------------------------------------------------------*
*&      Form  f_get_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_detail .
  data:
    lt_zdimwsbk type table of zdimwsbk,
    l_zdimwsbk  type zdimwsbk.
  data:
    lt_zdimbsbs type table of zdimbsbs,
    l_zdimbsbs  type zdimbsbs,
    l_bsis      type bsis,
    lt_bsis     type table of bsis,
    l_bsas      type bsas,
    lt_bsas     type table of bsas.

  if v_bukrs <> bukrs or
     v_hkont <> hkont or
     v_gjahr <> gjahr or
     v_monat <> monat or
     v_uscod <> uscod  ""
     or v_rldnr <> rldnr
    .

    clear itab[].
****查找企业未达明细—赋itab
    select *
      from zdimwsbk
      into corresponding fields of table lt_zdimwsbk
     where bukrs = bukrs
       and gjahr <= gjahr
       and hkont = w_hkont
       and zdimuscod = uscod""
       and monat < monat
       and zdimflag = space.
    loop at lt_zdimwsbk into l_zdimwsbk.
      itab-zdimline = l_zdimwsbk-zdimline.
      itab-zdimcid = l_zdimwsbk-zdimcid.
      itab-zdimcscod = l_zdimwsbk-zdimcscod.
      itab-uname = l_zdimwsbk-zdimname.
      itab-zdimedate = l_zdimwsbk-zdimedate.
      itab-shkzg = l_zdimwsbk-shkzg.
      itab-chgty = '1'.
      itab-dmbtr = l_zdimwsbk-dmbtr.
      itab-zdimdtext = l_zdimwsbk-zdimdtext.
      append itab.
    endloop.

****查找银行未达明细
    select *
      from zdimbsbs
      into corresponding fields of table lt_zdimbsbs
     where bukrs = bukrs
       and gjahr <= gjahr
       and hkont = w_hkont
       and monat < monat
       and zdimuscod = uscod""
       and zdimflag = space.
    loop at lt_zdimbsbs into l_zdimbsbs.
      itab-zdimline = l_zdimbsbs-buzei.
      itab-zdimcid = l_zdimbsbs-belnr.
      itab-zdimcscod = space.
      itab-uname = space.
      itab-zdimedate = l_zdimbsbs-bldat.
      itab-shkzg = l_zdimbsbs-shkzg.
      itab-chgty = '0'.
      itab-dmbtr = l_zdimbsbs-dmbtr.
      itab-zdimdtext = l_zdimbsbs-sgtxt.
      append itab.
    endloop.
*   保留画面输入
    v_bukrs = bukrs.
    v_hkont = hkont.
    v_gjahr = gjahr.
    v_monat = monat.
    v_rldnr = rldnr.

    v_uscod = uscod.""
    v_itab[] = itab[].
  endif.

endform.                    " f_get_detail
*&---------------------------------------------------------------------*
*&      Form  pur_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pur_check .
  data : msg     type string,
         p_hkont type zdimbuhk-hkont.
*------------guwen482---add-AUTHORITY-CHECK---20090114-------------------------

  data: begin of i_bukrs occurs 0,
          bukrs like t001k-bukrs,
          werks like t001k-bwkey, "评估范围
        end of i_bukrs.
  data:g_str type string.

  select bukrs
*         bwkey AS werks
  into table i_bukrs
  from t001
  where bukrs = bukrs
  .
  if lines( i_bukrs ) = 0.
    message '请重新选择条件，不存在公司条件' type 'I'.
    exit."STOP.
  else.
    loop at i_bukrs.
      authority-check object 'Z_AUTH_CHK'
               id 'BUKRS' field i_bukrs-bukrs
*               ID 'WERKS' FIELD i_bukrs-werks
*               ID 'VKORG' FIELD '*'
*               ID 'VTWEG' FIELD '*'
*               ID 'EKORG' FIELD '*'
               .
      g_str = i_bukrs-bukrs.
      if sy-subrc ne 0.
        concatenate  '没有' g_str '公司的权限' into g_str.
        message g_str type 'E'.
*      "STOP.
      endif.
    endloop.
  endif.
*-------------------------------------------------------------------------------
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = hkont
    importing
      output = p_hkont.
  authority-check object 'Z_FI_BANK' id 'BUKRS' field bukrs
                                     id 'SAKNR' field p_hkont.
  if sy-subrc <> 0.
    concatenate '你没有公司代码' bukrs
                '及其对应科目' hkont '的权限！' into msg.
    message i888(sabapdocu) with msg.
*    STOP.
  endif.

endform.                    " pur_check
*&---------------------------------------------------------------------*
*&      Form  f_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_check .

  data:
    l_zdimbuhk type zdimbuhk,
    l_hkont    type char10.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = hkont
    importing
      output = l_hkont.

  select single *
    from zdimbuhk
    into l_zdimbuhk
   where bukrs = bukrs
     and hkont = l_hkont
     and zdimuscod = uscod
     and zdimflag2 = 'X' .
  if sy-subrc <> 0.
    message e888(sabapdocu) with  'ERROR:科目与账号没有维护'.

    "LEAVE PROGRAM.
  endif.

endform.                    " f_check
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'GUISTAT'.
  set titlebar 'GUITIT'.


endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  get_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_lines output.
  data:
      line type i.

  describe table itab lines line.
* 控制tc行数跟内表一样
  "G_TC0100_LINES = line.
  tc0100-lines = line.

* 设定输出可否输入的范围设定
  if flg_chg = 'X'.
    loop at screen.
      if screen-name = 'DMBTR2'.

        screen-input = '1'.
        modify screen.
      endif.
    endloop.
  else.
    loop at screen.
      if screen-name = 'DMBTR2'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  endif.

endmodule.                 " get_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_itab  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_itab input.
**** 按屏幕输入值更改内表
  modify itab index tc0100-current_line.

endmodule.                 " modify_itab  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_add_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_add_item .
  data:
    l_line     type i,
    l_zdimline type zdimwseg-zdimline.

  if flg_chg = 'X'.
*   行号设定
    sort itab by zdimline.
    describe table itab lines l_line.
    read table itab index l_line.
*   保留目前最大行号
    l_zdimline = itab-zdimline.
    clear itab.
    itab-zdimline = l_zdimline + 1.
    append itab.
  endif.

endform.                    " f_add_item
*&---------------------------------------------------------------------*
*&      Form  f_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_lock .
*  IF flg_lock = space.
*
*    PERFORM f_lock_zdimglt0.
*    PERFORM f_lock_zdimwkpf.
**    PERFORM f_lock_zdimwseg.
*
**    PERFORM f_lock_zdimbsbs.
**    PERFORM f_lock_zdimwsbk.
*
*  ENDIF.
*
*  flg_lock = 'X'.

endform.                    " f_lock
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimglt0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_lock_zdimglt0 .
  call function 'ENQUEUE_EZZDIMGLT0'
    exporting
      mode_zdimglt0  = 'X'
      mandt          = sy-mandt
      bukrs          = bukrs
      hkont          = w_hkont
      zdimuscod      = uscod
      gjahr          = gjahr
    exceptions
      foreign_lock   = 1
      system_failure = 2
      others         = 3.

  if sy-subrc <> 0.
    message i888(sabapdocu) with  'ERROR:银行期末余额锁表失败'.
    leave program.
  endif.


endform.                    " f_lock_zdimglt0
*&---------------------------------------------------------------------*
*&      Module  SET_D0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_d0100 output.
*初始化之前控制可输入范围
  if flg_mdf is initial.
    loop at screen .
      if tc0100-current_line > tc0100-lines.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  else.
*  初始化完成之后不可输入的范围

    loop at screen .

      if screen-name = 'BUKRS' or
         screen-name = 'HKONT' or
         screen-name = 'GJAHR' or
         screen-name = 'MONAT'.

        screen-input = '1'.
      else.
        screen-input = '0'.
      endif.

      modify screen.
    endloop.
  endif.

endmodule.                 " SET_D0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SHOWF4_SHKZG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module showf4_shkzg input.

  refresh values_tab1.
  clear values_tab1.

  values_tab1-shkzg = 'S'.
  values_tab1-txt = '借方'.
  append values_tab1.clear values_tab1.
  values_tab1-shkzg = 'H'.
  values_tab1-txt = '贷方'.
  append values_tab1.clear values_tab1.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'SHKZG'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ITAB-TXT'  "屏幕上的名称
      value_org   = 'S'
    tables
      value_tab   = values_tab1.

endmodule.                 " SHOWF4_SHKZG  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHOWF4_CHGTY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module showf4_chgty input.
  refresh values_tab2.
  clear values_tab2.

  values_tab2-chgty = '0'.
  values_tab2-txt = '银行未达'.
  append values_tab2.clear values_tab2.
  values_tab2-chgty = '1'.
  values_tab2-txt = '企业未达'.
  append values_tab2.clear values_tab2.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'CHGTY'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ITAB-CHGTY'  "屏幕上的名称
      value_org   = 'S'
    tables
      value_tab   = values_tab2.

endmodule.                 " SHOWF4_CHGTY  INPUT
*&---------------------------------------------------------------------*
*&      Form  f_get_account
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_account .
  data:
    l_account_balance type bapi1028_3-balance,
    l_account         type table of zbapi_acc,
    l_temp            type zbapi_acc,
    l_monat           type rpmax.
  clear:
    l_account_balance,
    l_account,
    l_temp,
    l_monat.

  l_temp-gl_account = w_hkont.
  append l_temp to l_account.

  l_monat = monat - 1.

  call function 'ZDIMENSION_FED_GET_SUB_BALAN_A'
    exporting
      rclnt           = sy-mandt
      bukrs           = bukrs
      ryear           = gjahr
      rpmax           = l_monat
      uscod           = uscod
      rldnr           = rldnr
    importing
      account_balance = l_account_balance
    tables
      account         = l_account.

  if sy-subrc = 0.
    dmbtr1 = l_account_balance.
  endif.

endform.                    " f_get_account

*&---------------------------------------------------------------------*
*&      Form  f_get_uscod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_uscod .
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = hkont
    importing
      output = w_hkont.

  "090511 by d00194
*  SELECT SINGLE zdimuscod
*    INTO uscod
*    FROM zdimbuhk
*   WHERE bukrs = bukrs
*     AND hkont = w_hkont.

endform.                    " f_get_uscod
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimwkpf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_lock_zdimwkpf .
  data:
    l_monat type zdimwsbk-monat.
  l_monat = monat.
  call function 'ENQUEUE_EZZDIMWSBK'
    exporting
      mode_zdimwsbk  = 'X'
      mandt          = sy-mandt
      bukrs          = bukrs
      zdimuscod      = uscod
      "gjahr          = gjahr
      "monat          = l_monat
    exceptions
      foreign_lock   = 1
      system_failure = 2
      others         = 3.
  if sy-subrc <> 0.
    message i888(sabapdocu) with  'ERROR:银行流水账状态表锁表失败'.
    leave program.
  endif.


endform.                    " f_lock_zdimwkpf
*&---------------------------------------------------------------------*
*&      Form  f_lock_zdimwseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_lock_zdimwseg .
  data:
    l_monat type zdimwseg-monat.

  l_monat = monat.
  call function 'ENQUEUE_EZZDIMWSEG'
    exporting
      mode_zdimwseg  = 'X'
      mandt          = sy-mandt
      bukrs          = bukrs
      gjahr          = gjahr
      monat          = l_monat
      uscod          = uscod
    exceptions
      foreign_lock   = 1
      system_failure = 2
      others         = 3.
  if sy-subrc <> 0.
    message i888(sabapdocu) with  'ERROR:银行流水账明细表锁表失败'.
    leave program.
  endif.


endform.                    " f_lock_zdimwseg
*&---------------------------------------------------------------------*
*&      Module  check_hkont_authority  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_hkont_authority input.
*     权限检查
  perform pur_check.
endmodule.                 " check_hkont_authority  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_BUKRS_authority  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_bukrs_authority input.
  case ok_code.
    when 'EXIT' or 'BACK' or 'CANC'.
      leave program.
  endcase.
*     维护检查
  perform f_check.
endmodule.                 " check_BUKRS_authority  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHOWF4_USCOD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module showf4_uscod input.
  perform frm_get_zdimuscod  using  uscod.
endmodule.                 " SHOWF4_USCOD  INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_zdimuscod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_USCOD  text
*----------------------------------------------------------------------*
form frm_get_zdimuscod  using
      p_zuscod like zdimbuhk-zdimuscod.
  data: begin of l_it_zuscod occurs 0,
          bukrs     like zdimbuhk-bukrs,
          hkont     like zdimbuhk-hkont,
          txt50     like zdimbuhk-txt50,
          zdimuscod like zdimbuhk-zdimuscod,
          uscodtxt  like zdimbuhk-txt50,
        end of l_it_zuscod.

  data: i_bukrs like bseg-bukrs.
  data: begin of dynpfields occurs 5.
      include structure dynpread.
  data: end of dynpfields.
  data: return_tab like  table of ddshretval with header line.
  field-symbols: <fs_zuscod> like l_it_zuscod.
  clear: l_it_zuscod[],
         l_it_zuscod.
  move 'BUKRS' to dynpfields-fieldname.
  append dynpfields.
  call function 'DYNP_VALUES_READ'
    exporting
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    tables
      dynpfields           = dynpfields
    exceptions
      invalid_abapworkarea = 01
      invalid_dynprofield  = 02
      invalid_dynproname   = 03
      invalid_dynpronummer = 04
      invalid_request      = 05
      no_fielddescription  = 06
      undefind_error       = 07.

  if sy-subrc eq 0. "nur falls Transport-Felder im Dynpro existieren
    read table dynpfields with key 'BUKRS'.
    if sy-subrc = 0.
      i_bukrs = dynpfields-fieldvalue.
    endif.
  endif.
  select * into corresponding fields of table l_it_zuscod
    from zdimbuhk
    where bukrs = i_bukrs
    and zdimflag2 = 'X'
    .
  select single *
    from t001
    where bukrs = i_bukrs.
  loop at l_it_zuscod assigning <fs_zuscod>.
    select single txt50 into <fs_zuscod>-txt50
      from skat
      where spras = sy-langu
        and ktopl = t001-ktopl
        and saknr = <fs_zuscod>-hkont.
    select single txt50 into <fs_zuscod>-uscodtxt
      from zdimbno
      where zdimuscod = <fs_zuscod>-zdimuscod.
  endloop.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'ZDIMUSCOD'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'USCOD'  "屏幕上的名称
      value_org       = 'S'
    tables
      value_tab       = l_it_zuscod
      return_tab      = return_tab
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    read table return_tab index 1.
    p_zuscod = return_tab-fieldval.

  endif.
endform.                    " FRM_GET_zdimuscod
*&---------------------------------------------------------------------*
*&      Form  f_get_ldgrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_ldgrp .
  if bukrs is not initial.
    select single rldnr into rldnr
      from zdimldgrp
      where bukrs = bukrs.
    if sy-subrc <> 0.
      message e888(sabapdocu) with  '未取得分类账'.
    endif.
    "ELSE."IF bukrs IS NOT INITIAL

  endif."IF bukrs IS NOT INITIAL

endform.                    " f_get_ldgrp
*&---------------------------------------------------------------------*
*&      Form  f_check_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_check_init .
  data:
    l_zdimwseg type zdimwseg,
    l_text     type string,
    l_monat    type rpmax.

  l_monat = monat - 1.
**** 已初始其他月份检查
  select * up to 1 rows
    from zdimwseg
    into corresponding fields of l_zdimwseg
   where bukrs = bukrs
     and gjahr = gjahr
     and zdimuscod = uscod
     and monat <> l_monat.
  endselect.

  if sy-subrc = 0.

    concatenate '只能初始一个月的期初值，请先将'
                l_zdimwseg-monat
                '月的账单撤销！'
           into l_text.
    message e888(sabapdocu) with  l_text.
    stop.

  endif.

endform.                    " f_check_init
*&---------------------------------------------------------------------*
*&      Form  f_get_txt50
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_txt50 .
*by d00194
*SELECT SINGLE txt50  "总账科目描述
*    FROM zdimbuhk
*    INTO txt50
*   WHERE bukrs = bukrs
*     AND hkont = w_hkont
*     AND zdimuscod = uscod.

endform.                    " f_get_txt50
*&---------------------------------------------------------------------*
*&      Form  f_check_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_check_update .
  data:
    lt_zdimwsbk type table of zdimwsbk,
    l_zdimwsbk  type zdimwsbk.

  select * up to 1 rows
    from zdimwsbk
    into corresponding fields of l_zdimwsbk
   where bukrs = bukrs
     and zdimuscod = uscod
     and zdimflag = 'X'.
  endselect.

  if sy-subrc = 0.
    message e888(sabapdocu) with
                            'ERROR:请先将单据取消对帐'.
*    STOP.
  endif.

endform.                    " f_check_update
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_hkont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_USCOD  text
*----------------------------------------------------------------------*
form frm_get_hkont  using p_bukrs like bukrs
                          p_uscod like uscod.

  select single hkont into hkont
    from zdimbuhk
    where bukrs = p_bukrs
      and zdimuscod = p_uscod.
  if sy-subrc ne 0.
    message e888(sabapdocu) with text-111.
  endif.
endform.                    " FRM_GET_hkont
" get_hkont  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_USCOD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_uscod input.
  perform frm_get_hkont using bukrs uscod.
endmodule.                 " CHECK_USCOD  INPUT
