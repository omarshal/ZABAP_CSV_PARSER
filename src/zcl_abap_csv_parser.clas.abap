class zcl_abap_csv_parser definition
  public
  final
  create public .

public section.

  interfaces ZIF_abap_CSV_PARSER .

  aliases PARSE_FILE_TO_TABLE
    for ZIF_abap_CSV_PARSER~PARSE_FILE_TO_TABLE .

  methods CONSTRUCTOR
    importing
      !IV_FILE_PATH type STRING
    raising
      ZCX_abap_CSV_PARSER .
protected section.
private section.

  types:
    tty_file_data TYPE STANDARD TABLE OF string WITH DEFAULT KEY .

  data MV_FILE_PATH type STRING .
  data:
    mt_file_data TYPE STANDARD TABLE OF string .

  methods READ_FILE
    returning
      value(RT_FILE_DATA) type TTY_FILE_DATA
    raising
      ZCX_abap_CSV_PARSER .
  methods GET_FILE_DATA
    returning
      value(RT_FILE_DATA) type TTY_FILE_DATA .
ENDCLASS.



CLASS zcl_abap_csv_parser IMPLEMENTATION.


METHOD constructor.
    mv_file_path = iv_file_path.
    mt_file_data = me->read_file( ).
  ENDMETHOD.


METHOD get_file_data.
    rt_file_data = mt_file_data.
  ENDMETHOD.


METHOD parse_file_to_table.
    FIELD-SYMBOLS: <lv_field_value> TYPE any,
                   <lt_parsed_data> TYPE STANDARD TABLE.
    DATA: ls_struct_data TYPE REF TO data.
    DATA(lr_struct_type) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( iv_struc_name ) ).
    DATA(lt_struct_fields) = lr_struct_type->get_ddic_field_list( ).

    DATA(lt_file_data) = me->get_file_data( ).
    IF iv_has_header = abap_true AND lines( lt_file_data ) > 0.

      DATA(lv_header_row) = lt_file_data[ 1 ].
      lv_header_row = COND #( WHEN iv_add_client = abap_true THEN iv_separator && lv_header_row  "Add MANDT column
                              ELSE lv_header_row ).
      lv_header_row = COND #( WHEN substring( val = lv_header_row off = strlen( lv_header_row ) - 1 len = 1 ) = iv_separator
                              THEN lv_header_row && iv_separator  "If last column is empty, add 1 more separator otherwise split misses 1 line
                              ELSE lv_header_row ).
      lv_header_row = to_upper( lv_header_row ). "Upper case to be able to MAP fields

*-    When header available, validate columns (Compare number of columns)
      SPLIT lv_header_row AT iv_separator INTO TABLE DATA(lt_header_row_values).


      IF lines( lt_struct_fields ) <> lines( lt_header_row_values ) AND iv_map_fields = abap_false.
        "Error parsing file: Invalid CSV columns or unexpected separator
        RAISE EXCEPTION TYPE zcx_abap_csv_parser MESSAGE e002. " WITH TEXT-001.
      ENDIF.

      DELETE lt_file_data INDEX 1. "Remove header row
    ENDIF.

    CREATE DATA rt_parsed_data TYPE TABLE OF (iv_struc_name).
    ASSIGN rt_parsed_data->* TO <lt_parsed_data>.
    LOOP AT lt_file_data ASSIGNING FIELD-SYMBOL(<lv_file_row>).
      <lv_file_row> = COND #( WHEN iv_add_client = abap_true THEN iv_separator && <lv_file_row>  "Add MANDT column
                              ELSE <lv_file_row> ).
      SPLIT <lv_file_row> AT iv_separator INTO TABLE DATA(lt_file_row_values).
*      IF lines( lt_struct_fields ) <> lines( lt_file_row_values ).
*        "Error parsing file: Invalid CSV columns or unexpected separator
*        RAISE EXCEPTION TYPE zcx_crm_csv_parser MESSAGE e002 WITH text-001.
*      ENDIF.

      "Create a row for the given type an map the file values based on the position of the column
      APPEND INITIAL LINE TO <lt_parsed_data> ASSIGNING FIELD-SYMBOL(<ls_parsed_data>).
      LOOP AT lt_struct_fields ASSIGNING FIELD-SYMBOL(<lv_struct_field>).
        ASSIGN COMPONENT <lv_struct_field>-fieldname OF STRUCTURE <ls_parsed_data> TO <lv_field_value>.
        TRY.
            IF iv_map_fields = abap_true.
              DATA(field_index) = line_index( lt_header_row_values[ table_line = <lv_struct_field>-fieldname ] ).
              <lv_field_value> = lt_file_row_values[ field_index ].
            ELSE.
              <lv_field_value> = lt_file_row_values[ sy-tabix ].
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            "Last field is not always available (when empty).
        ENDTRY.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


METHOD read_file.
    cl_gui_frontend_services=>gui_upload(
       EXPORTING
         filename                = mv_file_path
      CHANGING
        data_tab                =  rt_file_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_csv_parser MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
