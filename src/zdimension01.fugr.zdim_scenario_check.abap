FUNCTION ZDIM_SCENARIO_CHECK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     REFERENCE(RETURN) LIKE  ZDIMSCNRO STRUCTURE  ZDIMSCNRO
*"  EXCEPTIONS
*"      NO_DATA
*"      ERROR_FIELD
*"----------------------------------------------------------------------
select single * into return
  from zdimscnro.
if sy-subrc ne 0.
  RAISE NO_DATA.
else.
 if return-ZDIMSCSL eq 'Y'.
   select single *
     from dd03l
     where TABNAME = 'BSEG'
      and FIELDNAME = return-FIELDNAME.
 if sy-subrc ne 0.
   RAISE ERROR_FIELD.
 endif.
 else.
   clear return-FIELDNAME.
 endif.
endif.



ENDFUNCTION.
