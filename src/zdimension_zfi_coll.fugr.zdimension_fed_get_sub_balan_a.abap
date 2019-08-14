FUNCTION ZDIMENSION_FED_GET_SUB_BALAN_A.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(RCLNT) TYPE  MANDT OPTIONAL
*"     VALUE(BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(RYEAR) TYPE  GJAHR OPTIONAL
*"     VALUE(RPMAX) TYPE  RPMAX OPTIONAL
*"     VALUE(USCOD) TYPE  ZDIMUSCOD OPTIONAL
*"     VALUE(RLDNR) TYPE  FAGL_RLDNR OPTIONAL
*"  EXPORTING
*"     VALUE(ACCOUNT_BALANCE) LIKE  BAPI1028_3-BALANCE
*"  TABLES
*"      ACCOUNT STRUCTURE  ZBAPI_ACC OPTIONAL
*"      BALANCE STRUCTURE  ZBAPI_ACC_BAL OPTIONAL
*"----------------------------------------------------------------------

  data:V_ENDVT like glt0-TSLVT."月末余额
  RANGES:
   G_RANGE   FOR GLT0-RACCT.

  loop at ACCOUNT .
    G_RANGE-SIGN = 'I'.
    G_RANGE-OPTION = 'EQ'.
    G_RANGE-LOW = ACCOUNT-GL_ACCOUNT.
    APPEND G_RANGE.
  endloop.

  Data:L_OLD_RACCT TYPE GLT0-RACCT,
       L_NEW_RACCT TYPE GLT0-RACCT.
  DATA L_YS(2) TYPE N.                     "需汇总的月份数
  DATA L_FIELD(13).                        "表字段名
  DATA:L_REC TYPE I .                      "记录指针
  Data L_MOD type I.                       "
  FIELD-SYMBOLS <FS>.                      "用于动态运算的指针


  CLEAR L_OLD_RACCT.
  CLEAR L_NEW_RACCT.
IF SY-SAPRL < 700.
  select *   FROM glt0
    where BUKRS = BUKRS AND  RYEAR = RYEAR AND RACCT
IN G_RANGE AND glt0~RCLNT = SY-MANDT
    AND RLDNR = RLDNR
    .

    CLEAR L_YS.
    L_NEW_RACCT = GLT0-RACCT.
    L_REC = L_REC + 1 .
    L_MOD = L_REC mod 2.
    IF L_NEW_RACCT = L_OLD_RACCT OR L_REC <> 0.
      V_ENDVT = V_ENDVT + GLT0-HSLVT.
      DO RPMAX TIMES.
        CLEAR L_FIELD.
        L_YS = L_YS + 1.
        CONCATENATE 'GLT0-HSL' L_YS INTO L_FIELD.
        ASSIGN (L_FIELD) TO <FS>.
        V_ENDVT = V_ENDVT + <FS>.
      ENDDO.
    ENDIF.

    CONCATENATE 'GLT0-HSL' L_YS INTO L_FIELD.
    ASSIGN (L_FIELD) TO <FS>.
    if glt0-DRCRK = 'S'."计算本月借方余额
      BALANCE-GL_ACCOUNT = L_NEW_RACCT.
      BALANCE-BALANCE_S = <FS>.
    ELSE."计算本月贷方余额
      BALANCE-BALANCE_H = <FS>.
    ENDIF.
    "IF L_MOD = 0 .
      "BALANCE-GL_ACCOUNT = L_NEW_RACCT.
*        BALANCE-BALANCE = V_ENDVT.
      "BALANCE-BALANCE = BALANCE-BALANCE_S + BALANCE-BALANCE_H.
      "BALANCE-BALANCE_ADD = V_ENDVT.
      "APPEND BALANCE.
      ACCOUNT_BALANCE = ACCOUNT_BALANCE + V_ENDVT.
      clear V_ENDVT.
    "ENDIF.

    L_OLD_RACCT = L_NEW_RACCT.
    CLEAR L_NEW_RACCT.
  endselect.

ELSE."IF SY-SAPRL < 700.
  "检查货币与本位币是否一致
  DATA: t001 TYPE t001,
        skb1 TYPE skb1,
        i_flag.


  IF sy-subrc <> 0.
    exit.
  ENDIF.

  select single *
    from t001
    where bukrs = bukrs.
  select single *
    from skb1
    where bukrs = bukrs
      and SAKNR IN G_RANGE.
  if t001-waers eq skb1-waers.
    i_flag = 'X'.
  else.
    clear  i_flag.
  endif.
  CLEAR: t001, skb1.
  "end of 检查货币与本位币是否一致


  data: g_fieldname like dd03l-fieldname.
  data wa_zdimscnro TYPE zdimscnro.

  select single *
  from zdimscnro INTO wa_zdimscnro
  where ZDIMSCSL eq 'Y'.
 if sy-subrc ne 0.
   message e888(sabapdocu) WITH '本程序无运行条件'.
   exit.
 else.
  select single fieldname into g_fieldname
     from dd03l
     where TABNAME = 'BSEG'
      and FIELDNAME = wa_zdimscnro-FIELDNAME.
 if sy-subrc ne 0.
   message e888(sabapdocu) WITH '请正确维护表ZDIMSCNRO中字段名的值'.
   exit.
 endif.
 endif.

DATA where_code TYPE string.
CONCATENATE 'RBUKRS = BUKRS AND  RYEAR = RYEAR AND RACCT IN G_RANGE AND RLDNR = RLDNR AND'
g_fieldname 'eq USCOD' INTO where_code SEPARATED BY space.

  SELECT * FROM FAGLFLEXT
           INTO CORRESPONDING FIELDS OF TABLE it_glt0
    where
    (where_code)
*for dynamic field
*    RBUKRS = BUKRS
*    AND  RYEAR = RYEAR
*    AND RACCT IN G_RANGE
*    AND RLDNR = RLDNR
*    AND ZZ0001 eq USCOD
    .

  LOOP AT it_glt0 .

    CLEAR L_YS.
    L_NEW_RACCT = it_glt0-RACCT.
    L_REC = L_REC + 1 .
    L_MOD = L_REC mod 2.
    IF L_NEW_RACCT = L_OLD_RACCT OR L_REC <> 0.
      V_ENDVT = V_ENDVT + it_glt0-HSLVT.
      DO RPMAX TIMES.
        CLEAR L_FIELD.
        L_YS = L_YS + 1.
        IF i_flag eq 'X'.
          CONCATENATE 'it_glt0-HSL' L_YS INTO L_FIELD.
        ELSE.
          CONCATENATE 'it_glt0-TSL' L_YS INTO L_FIELD.
        ENDIF.

        ASSIGN (L_FIELD) TO <FS>.
        V_ENDVT = V_ENDVT + <FS>.
      ENDDO.
    ENDIF.


    "by d00194 @ 20090512
*    IF i_flag eq 'X'.
*          CONCATENATE 'it_glt0-TSL' L_YS INTO L_FIELD.
*        ELSE.
*          CONCATENATE 'it_glt0-HSL' L_YS INTO L_FIELD.
*        ENDIF.
*    ASSIGN (L_FIELD) TO <FS>.
*    if it_glt0-DRCRK = 'S'."计算本月借方余额
*      BALANCE-GL_ACCOUNT = L_NEW_RACCT.
*      BALANCE-BALANCE_S = <FS>.
*    ELSE."计算本月贷方余额
*      BALANCE-BALANCE_H = <FS>.
*    ENDIF.

    "IF L_MOD = 0 .
      "BALANCE-GL_ACCOUNT = L_NEW_RACCT.
*        BALANCE-BALANCE = V_ENDVT.
      "BALANCE-BALANCE = BALANCE-BALANCE_S + BALANCE-BALANCE_H.
      "BALANCE-BALANCE_ADD = V_ENDVT.
      "APPEND BALANCE.
      ACCOUNT_BALANCE = ACCOUNT_BALANCE + V_ENDVT.
      clear V_ENDVT.
    "ENDIF.

    L_OLD_RACCT = L_NEW_RACCT.
    CLEAR L_NEW_RACCT.
  ENDLOOP.

ENDIF."IF SY-SAPRL < 700.
ENDFUNCTION.
