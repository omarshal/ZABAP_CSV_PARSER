INTERFACE zif_abap_csv_parser
  PUBLIC .

  METHODS parse_file_to_table
    IMPORTING
      !iv_separator         TYPE c
      !iv_add_client        TYPE abap_bool
      !iv_has_header        TYPE abap_bool
      !iv_map_fields        TYPE abap_bool
      !iv_struc_name        TYPE string
    RETURNING
      VALUE(rt_parsed_data) TYPE REF TO data
    RAISING
      zcx_abap_csv_parser.

ENDINTERFACE.
