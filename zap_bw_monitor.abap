"#autoformat
**********************************************************************
* Author: T.Meyer, https://www.reyemsaibot.com, 2021-11-22
**********************************************************************
*
* Little BW Monitor to watch ADSO records and size
*
**********************************************************************
* Change log
**********************************************************************
* 22.11.21 TM initial version
**********************************************************************
REPORT zap_bw_monitor.
TYPES: BEGIN OF bw_monitor,
         object            TYPE string,
         table_type        TYPE string,
         table_name        TYPE string,
         table_description TYPE string,
         record_count      TYPE int8,
         size_mb           TYPE decimals,
       END OF bw_monitor.

DATA: bw_monitor_line TYPE bw_monitor.
DATA: bw_monitor_output TYPE TABLE OF bw_monitor.
DATA: table_length TYPE cctablen.
DATA(adso_wherecondition) = VALUE rs_t_range( ( sign = 'I'
                                                opt = 'EQ'
                                                low = 'AQ' )
                                              ( sign = 'I'
                                                opt = 'EQ'
                                                low = 'AT' )
                                              ( sign = 'I'
                                                opt = 'EQ'
                                                low = 'CL' )
                                            ).

SELECT adso~adsonm,
       description
  FROM rsoadso AS adso
  LEFT JOIN rsoadsot AS text
  ON adso~adsonm = text~adsonm
  INTO TABLE @DATA(adsos)
 WHERE adso~objvers = @rs_c_objvers-active
 AND text~objvers = @rs_c_objvers-active
  AND text~ttyp = 'EUSR'
  AND text~langu = 'E'.


DATA(entry_count) = lines( adsos ).

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING
    percentage = 0
    text       = |0% completed|.

DATA(count_index) = 1.

LOOP AT adsos ASSIGNING FIELD-SYMBOL(<adso>).

  bw_monitor_line-object = <adso>-adsonm.
  bw_monitor_line-table_description = <adso>-description.

  DATA(adso_tables) = cl_rso_adso=>get_tablnm( <adso>-adsonm ).

  LOOP AT adso_tables ASSIGNING FIELD-SYMBOL(<adso_table>) WHERE dsotabtype IN adso_wherecondition.
    CASE <adso_table>-dsotabtype.
      WHEN 'AQ'.
        bw_monitor_line-table_type = 'Advanced DSO Inbound Table'.
      WHEN 'AT'.
        bw_monitor_line-table_type = 'Advanced DSO Active Table'.
      WHEN 'CL'.
        bw_monitor_line-table_type = 'Advanced DSO Change Log'.
    ENDCASE.
    bw_monitor_line-table_name = <adso_table>-name.
    SELECT COUNT( * ) AS recordcount
    FROM (<adso_table>-name)
      INTO @bw_monitor_line-record_count.


    CALL FUNCTION 'SCCR_GET_NAMETAB_AND_TABLEN'
      EXPORTING
        tabname = <adso_table>-name
      IMPORTING
        tablen  = table_length
      EXCEPTIONS
        OTHERS  = 1.

    bw_monitor_line-size_mb = ( bw_monitor_line-record_count * table_length ) / ( 1024 * 1024 ).
    DATA(progress) = count_index * 100 / entry_count.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = |{ progress } % completed|.

    APPEND bw_monitor_line TO bw_monitor_output.
  ENDLOOP.
  count_index = count_index + 1.


ENDLOOP.

SORT bw_monitor_output BY size_mb DESCENDING.

cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv)
                        CHANGING  t_table      = bw_monitor_output ).
salv->get_columns( )->set_optimize( abap_true ).
salv->get_functions( )->set_all( abap_true ).
salv->get_columns( )->get_column( 'OBJECT' )->set_medium_text( 'Object' ).
salv->get_columns( )->get_column( 'TABLE_TYPE' )->set_medium_text( 'Table Type' ).
salv->get_columns( )->get_column( 'TABLE_NAME' )->set_medium_text( 'Table Name' ).
salv->get_columns( )->get_column( 'TABLE_DESCRIPTION' )->set_medium_text( 'Table Description' ).
salv->get_columns( )->get_column( 'RECORD_COUNT' )->set_medium_text( 'Record Count' ).
salv->get_columns( )->get_column( 'SIZE_MB' )->set_medium_text( 'Size in Mb' ).
salv->display( ).
