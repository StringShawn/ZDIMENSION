FUNCTION ZRFC_BANK_ACCOUNT_CHECKING.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZDIMUSCOD) TYPE  ZDIMBUHK-ZDIMUSCOD
*"     VALUE(WAERS) TYPE  CHAR20
*"  EXPORTING
*"     VALUE(RETURN) TYPE  CHAR01
*"     VALUE(MESSAGE) TYPE  CHAR50
*"  TABLES
*"      ZDIMBAAC STRUCTURE  ZDIMBAAC
*"----------------------------------------------------------------------
  if not ZDIMUSCOD is initial.
    select single *
      from ZDIMBUHK
      where ZDIMUSCOD = ZDIMUSCOD.
    if sy-subrc ne 0.
      message  = text-101.
      return = 'E'.
      exit.
    else.
      ZDIMWKPF-BUKRS = ZDIMBUHK-BUKRS.
     g_HKONT = ZDIMBUHK-HKONT.
     ZDIMWKPF-ZDIMUSCOD = ZDIMUSCOD.
     select single *
       from t001
       where BUKRS = ZDIMWKPF-BUKRS.

     select single txt50 into g_txt50
       from SKAT
       where SPRAS = sy-langu
       and KTOPL = t001-KTOPL
       and  SAKNR = g_HKONT.
    endif.
  else.
    message  = text-100.
    return = 'E'.
    exit.
  endif.
  if not WAERS is initial.
    SELECT SINGLE WAERS INTO ZDIMWKPF-WAERS
       FROM TCURT
       WHERE  KTEXT = WAERS
        OR  LTEXT = WAERS .
    if sy-subrc ne 0.
      message  = text-103.
      return = 'E'.
      exit.
    endif.
  else.
    message  = text-102.
    return = 'E'.
    exit.
  endif.
  zdimwkpf-ZDIMEDATE = sy-datum.
  clear: it_zdimwseg,
         it_zdimwseg[].
  if not  ZDIMBAAC[] is initial.
    read table ZDIMBAAC INDEX 1.
    g_gjahr = ZDIMBAAC-zdimedate(4).
    g_monat = ZDIMBAAC-ZDIMEDATE+4(2).
    loop at zdimbaac.
      MOVE-CORRESPONDING ZDIMBAAC to it_zdimwseg.
      if ZDIMBAAC-DMBTR01 NE 0.
        it_zdimwseg-SHKZG = 'S'.
        it_zdimwseg-DMBTR = ZDIMBAAC-DMBTR01.
      endif.
      if ZDIMBAAC-DMBTR02 NE 0.
        it_zdimwseg-SHKZG = 'H'.
        it_zdimwseg-DMBTR = ZDIMBAAC-DMBTR02.
      endif.
      it_zdimwseg-BUKRS = ZDIMWKPF-BUKRS.
      it_zdimwseg-GJAHR = g_gjahr.
      it_zdimwseg-MONAT = g_MONAT.
      it_zdimwseg-ZDIMUSCOD = ZDIMWKPF-ZDIMUSCOD.
      it_zdimwseg-WAERS  = ZDIMWKPF-WAERS.
      append it_zdimwseg.
      clear it_zdimwseg.
    endloop.
    PERFORM createnum.   "创建银行对账单流水ID
    perform frm_save_data CHANGING message
                                   return.
  else.
    message = text-104.
    return = 'E'.
    exit.
  endif.



ENDFUNCTION.
