FUNCTION z_sd_core_doc_flow_get_beu.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VBELN_VA) TYPE  VBELN_VA
*"  TABLES
*"      ET_FLOW STRUCTURE  ZSD_CORE_FLOW_S
*"--------------------------------------------------------------------
  DATA(lo_flow) = zcl_sd_core_doc_flow=>create( i_vbeln_va = iv_vbeln_va ).
  lo_flow->read_regular_flow_beu( ).
  lo_flow->read_ekbe( ).
*  lo_flow->read_rbkp( ).
  et_flow[] = lo_flow->mt_flow.

ENDFUNCTION.
