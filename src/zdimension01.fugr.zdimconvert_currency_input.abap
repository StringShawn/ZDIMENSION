FUNCTION ZDIMCONVERT_CURRENCY_INPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(VALUE) TYPE  ALSMEX_TABLINE-VALUE
*"     REFERENCE(ZDIMDCPFM) TYPE  ZDIMDCPFM
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  BSEG-DMBTR
*"  EXCEPTIONS
*"      WRONG_FORMAT_IN_INPUT
*"----------------------------------------------------------------------
data: DATA_SEP(1)     TYPE C,
      g_value like ALSMEX_TABLINE-VALUE,
      g_value02 like ALSMEX_TABLINE-value.
data:   BEGIN OF DATA_CHAR,
            NUM(12) TYPE C VALUE '-0123456789 ',
            SEP(1)  TYPE C,
            SED(1)  TYPE C,
            END OF DATA_CHAR.
DATA :BEGIN OF l_data OCCURS 0,
             data(7) TYPE c,
            END OF l_data.
clear output.
*check if a char is wrong in data
if ZDIMDCPFM eq 'W' or ZDIMDCPFM eq '' .
 DATA_CHAR-SED = '.'.
 IF VALUE CN DATA_CHAR.
    MESSAGE text-001 type 'S'  RAISING WRONG_FORMAT_IN_INPUT.
 endif.
elseif ZDIMDCPFM eq 'X'.
  DATA_CHAR-Sep = ','.
  DATA_CHAR-SED = '.'.
   IF VALUE CN DATA_CHAR.
    MESSAGE text-002 type 'S'  RAISING WRONG_FORMAT_IN_INPUT.
 endif.
elseif ZDIMDCPFM eq 'Y'.
  DATA_CHAR-Sep = ''.
  DATA_CHAR-SED = ','.
   IF VALUE CN DATA_CHAR.
    MESSAGE text-003 type 'S'  RAISING WRONG_FORMAT_IN_INPUT.
 endif.
elseif ZDIMDCPFM eq 'Z'.
  DATA_CHAR-Sep = '.'.
  DATA_CHAR-SED = ','.
    IF VALUE CN DATA_CHAR.
    MESSAGE text-002 type 'S'  RAISING WRONG_FORMAT_IN_INPUT.
 endif.
endif.
clear: l_data[],
       l_data,
       g_value.

case ZDIMDCPFM.
  when 'W' or ''. "N NNN NNN.NN
   output = value.
  when 'X'.         "N,NNN.NN
    SPLIT  value  AT ',' INTO TABLE l_data.
  CLEAR g_value.
  LOOP AT l_data.
    CONCATENATE g_value l_data-data INTO g_value.
  ENDLOOP.
  output = g_value.
  when 'Y'.       "NNN NNN,NN
    g_value = value.
    clear g_value02.
   SPLIT g_value AT ',' into g_value g_value02.
   if g_value ne space.
   CONCATENATE g_value '.' g_value02 into g_value.
    output = g_value.
   else.
    output = g_value.
   endif.
  when 'Z'.    "N.NNN,NN
   SPLIT  value  AT '.' INTO TABLE l_data.
  CLEAR g_value.
  LOOP AT l_data.
    CONCATENATE g_value l_data-data INTO g_value.
  ENDLOOP.
   clear g_value02.
   SPLIT g_value AT ',' into g_value g_value02.
   if g_value ne space.
   CONCATENATE g_value '.' g_value02 into g_value.
    output = g_value.
   else.
    output = g_value.
   endif.
endcase.


ENDFUNCTION.
