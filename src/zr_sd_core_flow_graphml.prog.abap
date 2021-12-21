**Please use the pattern Z_BC_PROGRAM_HEAD to create the program head**
REPORT zr_sd_core_flow_graphml.


PARAMETERS: p_type   TYPE zsd_vbtyp,
            p_number TYPE vbeln.



DATA(go_flow) = zcl_sd_core_doc_flow=>create(
                  iv_doctype = p_type
                  iv_number  = p_number
                ).


go_flow->read( ).


DATA lt_out TYPE stringtab.
lt_out = go_flow->to_graphviz( ).


CALL METHOD cl_gui_frontend_services=>gui_download
  EXPORTING
    filename                = 'test.gv'   " Name of file
  CHANGING
    data_tab                = lt_out    " Transfer table
  EXCEPTIONS
    file_write_error        = 1
    no_batch                = 2
    gui_refuse_filetransfer = 3
    invalid_type            = 4
    no_authority            = 5
    unknown_error           = 6
    header_not_allowed      = 7
    separator_not_allowed   = 8
    filesize_not_allowed    = 9
    header_too_long         = 10
    dp_error_create         = 11
    dp_error_send           = 12
    dp_error_write          = 13
    unknown_dp_error        = 14
    access_denied           = 15
    dp_out_of_memory        = 16
    disk_full               = 17
    dp_timeout              = 18
    file_not_found          = 19
    dataprovider_exception  = 20
    control_flush_error     = 21
    not_supported_by_gui    = 22
    error_no_gui            = 23
    OTHERS                  = 24.

IF sy-subrc > 0.
  MESSAGE |RC: { sy-subrc }| TYPE 'I'.
ENDIF.
