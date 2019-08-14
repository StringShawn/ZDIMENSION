*&---------------------------------------------------------------------*
*& Report  ZDIMENSION_FII007
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDIMENSION_FII007.

*&---------------------------------------------------------------------*
*& 声明系统透明表
*&---------------------------------------------------------------------*
TABLES: smen_buffc.

*&---------------------------------------------------------------------*
*& 定义变量与内表
*&---------------------------------------------------------------------*
* 功能码返回值
DATA:ok_code   TYPE sy-ucomm,
     save_ok   LIKE ok_code.

DATA node_table LIKE TABLE OF mtreesnode.      "输出到屏幕的结节点表
DATA node1    TYPE mtreesnode.                 "节点工作区
DATA i_result TYPE TABLE OF smen_buffc WITH HEADER LINE. "结果内表
DATA wa_result TYPE smen_buffc.
* 定制对象
DATA:wa_container TYPE scrfname VALUE 'TREE1',
* 客户对象
wa_custom_container TYPE REF TO cl_gui_custom_container,
wa_tree TYPE REF TO cl_gui_simple_tree.
* 声明RANGE变量
RANGES r_uname FOR smen_buffc-uname.      "用户名

DATA:BEGIN OF i_user OCCURS 0,
       uname     LIKE smen_buffc-uname,   "用户帐号
       indix(2)  TYPE n,                  "用户编码
       ltext(40) TYPE c,                  "用户文件夹名称
       id        LIKE smen_buffc-object_id, "文件夹ID
     END OF i_user.
DATA i_temp_id TYPE TABLE OF smen_buffc WITH HEADER LINE.


CLASS lcl_application DEFINITION DEFERRED.
*----------------------------------------------------------------------*
* CLASS LCL_APPLICATION DEFINITION
*----------------------------------------------------------------------*
* 对象定义
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
* 自定义双击方法，参数为节点关键字
    METHODS handle_node_double_click
          FOR EVENT node_double_click OF cl_gui_simple_tree
          IMPORTING node_key.
ENDCLASS.

*---------------------------------------------------------------------*
* CLASS LCL_APPLICATION IMPLEMENTATION
*---------------------------------------------------------------------*
* 对象方法实现
*---------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD handle_node_double_click.         "双击事件的处理方法
* 从节点内表中按关键字读取单个节点
    CLEAR node1.
    READ TABLE node_table WITH KEY node_key = node_key INTO node1.
    IF sy-subrc = 0.
      CLEAR i_result.
      READ TABLE i_result WITH KEY object_id = node_key
                              INTO wa_result.
    ENDIF.
* 调用相应的事务
    IF node1-isfolder NE 'X'.
      CALL TRANSACTION wa_result-report.  " AND RETURN.
*      CALL 'AUTH_CHECK_TCODE'
*           ID 'TCODE' FIELD wa_result-report.
*      IF sy-subrc = 0.
*        CALL TRANSACTION wa_result-report.
*      ELSE.
*        MESSAGE s999(zd00) WITH '您无权使用事务' wa_result-report.
*        EXIT.
*      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

DATA: g_application TYPE REF TO lcl_application.

*&--------------------------------------------------------------------*
*&  DEFINE m_uname. 定义宏：初始化关键用户信息
*&--------------------------------------------------------------------*
DEFINE m_uname.
  clear r_uname.
  r_uname-sign   = 'I'.
  r_uname-option = 'EQ'.
  r_uname-low    = &1.
  append r_uname.
  clear i_user.
  l_indix      = l_indix + 1.
  i_user-uname = &1.
  i_user-indix = l_indix.
  i_user-ltext = &2.
  append i_user.
END-OF-DEFINITION.
************************************************************************
*                      INITIALIZATION                                  *
************************************************************************
DATA return TYPE ZDIMSCNRO.
INITIALIZATION.


 CALL FUNCTION 'ZDIM_SCENARIO_CHECK'
  IMPORTING
    RETURN            = return
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
 ENDIF.



*&--------------------------------------------------------------------*
*& 执行程序事件
*&--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_query_smen_buffc.    "取符合条件的数据
*  PERFORM f_integrate_tree.      "对节点树进行筛选处理
  PERFORM f_init_tree.           "初始化节点树
  SET SCREEN '0100'.
*&---------------------------------------------------------------------*
*&      Form  f_query_smen_buffc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_query_smen_buffc .
DATA l_indix(2) TYPE n.
  DEFINE m_menu.
    clear i_result.
    i_result-uname = ''.
    i_result-reporttype = &1.
    i_result-report = &2.
    i_result-menu_level = '2'.
    i_result-parent_id = &3.
    i_result-object_id = &4.
    i_result-text = &5.
    append i_result.
  END-OF-DEFINITION.
IF return-ZDIMSCSL eq 'Y'.
  "m_menu 'TR' 'ZFIR01' '000001' '000002' '银行帐号与总帐科目关系表维护'.
  m_menu 'TR' 'ZDIMFIM004' '000001' '000003' '银行余额初始化'.
  m_menu 'TR' 'ZDIMFIM001' '000001' '000004' '银行对帐单上传与维护'.
  m_menu 'TR' 'ZDIMFIM002' '000001' '000005' '银行对帐处理'.
  m_menu 'TR' 'ZDIMFIM005' '000001' '000006' '对账单查询、取消与期间锁定'.
  m_menu 'TR' 'ZDIMFIM006' '000001' '000007' '银行调节余额报表'.
else.
  "m_menu 'TR' 'ZFIR01' '000001' '000002' '银行帐号与总帐科目关系表维护'.
  m_menu 'TR' 'ZDIMFIS004' '000001' '000003' '银行余额初始化'.
  m_menu 'TR' 'ZDIMFIS001' '000001' '000004' '银行对帐单上传与维护'.
  m_menu 'TR' 'ZDIMFIS002' '000001' '000005' '银行对帐处理'.
  m_menu 'TR' 'ZDIMFIS005' '000001' '000006' '对账单查询、取消与期间锁定'.
  m_menu 'TR' 'ZDIMFIS006' '000001' '000007' '银行调节余额报表'.
ENDIF.


ENDFORM.                    " f_query_smen_buffc
*&---------------------------------------------------------------------*
*&      Form  f_init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_init_tree .
* 需要按层次排序
  SORT i_result BY menu_level sort_order object_id.
  LOOP AT i_result.
    CLEAR node1.
    node1-node_key = i_result-object_id.
    node1-relatkey = i_result-parent_id.
    node1-relatship = cl_gui_simple_tree=>relat_last_child.
    node1-hidden = ''.
    node1-disabled = ''.
    node1-text = i_result-text+0(30).
    IF i_result-reporttype IS INITIAL.
      node1-isfolder = 'X'.
      node1-n_image  = '@FN@'.
      node1-exp_image = '@FN@'.
    ELSE.
      node1-n_image  = '@CS@'.
      node1-exp_image = '@CS@'.
    ENDIF.
    APPEND node1 TO node_table.
  ENDLOOP.
* 根节点单独处理
  CLEAR node1.
  node1-node_key = '00001'.
  CLEAR node1-relatkey.
  node1-relatship = cl_gui_simple_tree=>relat_last_child.
  node1-hidden = ''.
  node1-disabled = ''.
  node1-isfolder = 'X'.
  node1-n_image  = '@FN@'.
  node1-exp_image = '@FN@'.
  node1-dragdropid = '1'.
  CLEAR node1-expander.
  node1-text = '银行对帐程序'.    "节点文本
  INSERT node1 INTO node_table INDEX 1.

ENDFORM.                    " f_init_tree
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  SET PF-STATUS 'XAVERY'.
  SET TITLEBAR 'TITLE'.
* 如果窗口还没有创建TREE对象则创建它
  IF wa_custom_container IS INITIAL.
* 创建自定义对象
    CREATE OBJECT g_application.
    PERFORM create_tree.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_tree .
* 事件内表及单个事件对象
  DATA:events TYPE cntl_simple_events,
       event1 TYPE cntl_simple_event.
* 建立定制控制对象
  CREATE OBJECT wa_custom_container
      EXPORTING container_name = wa_container.
* 建立树对象
  CREATE OBJECT wa_tree
      EXPORTING
*         lifetime             =
          parent               = wa_custom_container
*         shellstyle           =
          node_selection_mode  =
                          cl_gui_simple_tree=>node_sel_mode_single.
*         hide_selection       =
*         name                 =
*     exceptions
*         lifetime_error       = 1
*         cntl_system_error    = 2
*         create_error         = 3
*         failed               = 4
*         illegal_node_selecton_mode = 5
*         others               = 6

  IF sy-subrc NE 0.
  ENDIF.

* 按照节点内表添加节点
  CALL METHOD wa_tree->add_nodes
      EXPORTING
        table_structure_name = 'MTREESNODE'
        node_table           = node_table.
  IF sy-subrc <> 0.
  ENDIF.
* 修改节点文本描述
  SORT i_result BY uname object_id .
  LOOP AT node_table INTO node1 WHERE node_key NE '00001'.
    CLEAR:i_user, i_result.
    READ TABLE i_user WITH KEY indix = node1-node_key+0(2).
    READ TABLE i_result WITH KEY uname     = i_user-uname
                                 object_id = node1-node_key+2(5)
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      IF i_result-reporttype EQ 'TR'.
        CONCATENATE i_result-report '-' i_result-text
               INTO i_result-text SEPARATED BY space.
      ENDIF.
      CALL METHOD wa_tree->node_set_text
         EXPORTING
           node_key = node1-node_key
           text     = i_result-text.
    ENDIF.
  ENDLOOP.

* 将节点展开
  CALL METHOD wa_tree->expand_node
    EXPORTING
      node_key                    = '00001'
      expand_subtree              = ' '        "不展开子节点
    EXCEPTIONS
      failed                      = 1
      illegal_level_count         = 2
      cntl_system_error           = 3
      node_not_found              = 4
      cannot_expand_leaf          = 5.
  IF sy-subrc <> 0.
  ENDIF.
*
* 定义双击事件
  event1-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  event1-appl_event = 'X'.
  APPEND event1 TO events.
*
* 建立菜单事件
  event1-eventid = cl_gui_simple_tree=>eventid_node_context_menu_req.
  event1-appl_event = ''.
  APPEND event1 TO events.
* 菜单选择有效
  CALL METHOD wa_tree->set_ctx_menu_select_event_appl
   EXPORTING
    appl_event = 'X'.

* 添加事件内表
  CALL METHOD wa_tree->set_registered_events
    EXPORTING
      events  = events.
* 将已定义的双击事件分配至树对象
  SET HANDLER g_application->handle_node_double_click FOR wa_tree.

ENDFORM.                    " create_tree
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
DATA str1 TYPE tv_nodekey.
  save_ok = ok_code.

  CLEAR ok_code.

* 退出按钮时退出程序
  CASE save_ok.
    WHEN 'EXIT' OR 'BACK' OR 'RWO'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      CALL METHOD wa_tree->get_selected_node
        IMPORTING
          node_key = str1.
*      exceptions
*        failed                     = 1
*        single_node+selection_only = 2
*        cntl_system_error          = 3
*        others                     = 4
      IF sy-subrc EQ 0.
*        getkey = str1.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
