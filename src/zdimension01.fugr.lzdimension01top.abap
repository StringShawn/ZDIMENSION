FUNCTION-POOL ZDIMENSION01               MESSAGE-ID SV.
*customize data defintion by d00194 at 090611
  tables : ZDIMWKPF  ,t001, dd03l, zdimglt0.
  data : g_HKONT  type ZDIMBUHK-HKONT ,
        g_txt50 type skat-txt50 ,
        G_GJAHR TYPE zdimwseg-gjahr ,
        G_MONAT TYPE zdimwseg-monat .

  DATA: it_zdimwseg TYPE TABLE OF zdimwseg WITH HEADER LINE,
        it_zdimwsbk TYPE TABLE OF zdimwsbk WITH HEADER LINE ,
        it_zdimglt0 TYPE TABLE OF zdimglt0 WITH HEADER LINE,
          itab1 like TABLE OF it_zdimwseg WITH HEADER LINE.
*end of customize data definition


  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZDIMENSION01T00                        . "view rel. data dcl.
