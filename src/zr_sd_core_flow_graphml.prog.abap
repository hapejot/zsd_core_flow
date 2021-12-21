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
*   bin_filesize            =     " File length for binary files
    filename                = 'test.gv'   " Name of file
*   filetype                = 'ASC'    " File type (ASCII, binary ...)
*   append                  = SPACE    " Character Field Length 1
*   write_field_separator   = SPACE    " Separate Columns by Tabs in Case of ASCII Download
*   header                  = '00'    " Byte Chain Written to Beginning of File in Binary Mode
*   trunc_trailing_blanks   = SPACE    " Do not Write Blank at the End of Char Fields
*   write_lf                = 'X'    " Insert CR/LF at End of Line in Case of Char Download
*   col_select              = SPACE    " Copy Only Selected Columns of the Table
*   col_select_mask         = SPACE    " Vector Containing an 'X' for the Column To Be Copied
*   dat_mode                = SPACE    " Numeric and date fields are in DAT format in WS_DOWNLOAD
*   confirm_overwrite       = SPACE    " Overwrite File Only After Confirmation
*   no_auth_check           = SPACE    " Switch off Check for Access Rights
*   codepage                =     " Character Representation for Output
*   ignore_cerr             = ABAP_TRUE    " Ignore character set conversion errors?
*   replacement             = '#'    " Replacement Character for Non-Convertible Characters
*   write_bom               = SPACE    " If set, writes a Unicode byte order mark
*   trunc_trailing_blanks_eol = 'X'    " Remove Trailing Blanks in Last Column
*   wk1_n_format            = SPACE
*   wk1_n_size              = SPACE
*   wk1_t_format            = SPACE
*   wk1_t_size              = SPACE
*   show_transfer_status    = 'X'    " Enables suppression of transfer status message
*   fieldnames              =     " Table Field Names
*   write_lf_after_last_line  = 'X'    " Writes a CR/LF after final data record
*   virus_scan_profile      = '/SCET/GUI_DOWNLOAD'    " Virus Scan Profile
*  IMPORTING
*   filelength              =     " Number of bytes transferred
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
