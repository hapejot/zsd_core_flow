FUNCTION Z_SD_CORE_DOC_FLOW_GET_BEU.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DOCTYPE) TYPE  ZSD_VBTYP
*"     VALUE(IV_VBELN) TYPE  VBELN
*"  TABLES
*"      ET_FLOW STRUCTURE  ZSD_CORE_FLOW_S
*"----------------------------------------------------------------------
  DATA(lo_flow) = zcl_sd_core_doc_flow=>create(
                        iv_doctype = iv_doctype
                        iv_number = iv_vbeln ).
  lo_flow->read_regular_flow_beu( ).
  lo_flow->read_ekbe( ).
*  lo_flow->read_rbkp( ).
  lo_flow->read_banfn( ).
  et_flow[] = lo_flow->mt_flow.

ENDFUNCTION.
