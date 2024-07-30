// Note: parser rules start with lowercase letters, lexer rules with uppercase

grammar SOQL;

// the select statement element
select_statement
  : select_clause from_clause (where_clause)? (orderby_clause)?
  ;

// the select clause
select_clause
  : 'SELECT' ('DISTINCT')? select_expression (',' select_expression)*
  ;

select_expression
  : chained_method_expression
  | object_declaration
  ;

chained_method_expression
  : IDENTIFICATION_VARIABLE '.' method_expression ('.' method_expression)*
  ;

method_expression
  : method_name method_argument
  ;

method_name
  : IDENTIFICATION_VARIABLE
  ;

method_argument
  : '()'
  | '(' var_reference (',' var_reference)* ')'
  ;

// the from clause parts
from_clause
  : 'FROM' entity_declaration (',' entity_declaration)*
  ;

entity_declaration
  : class_declaration object_declaration
  ;

class_declaration
  : IDENTIFICATION_VARIABLE
  ;

object_declaration
  : IDENTIFICATION_VARIABLE
  ;

// the where clause parts
where_clause
  : 'WHERE' or_expression
  ;

or_expression
  : (and_expression) ('OR' and_expression)*
  ;

and_expression
  : (expression_term) ('AND' expression_term)*
  ;

expression_term
  : ('NOT')? comparison_expression
  ;

comparison_expression
  : chained_method_expression comparison_operator (value_declaration | var_reference | chained_method_expression)
  ;

comparison_operator
  : '='
  | '>'
  | '>='
  | '<'
  | '<='
  | '<>'
  ;

var_reference
  :  ':' IDENTIFICATION_VARIABLE
  ;

value_declaration
  :  '"' IDENTIFICATION_VARIABLE '"'
  ;

// the ordering part
orderby_clause
  : 'ORDER' 'BY' orderby_item (',' orderby_item)*
  ;

orderby_item
  : chained_method_expression ('ASC' | 'DESC')?
  ;

// lexer rules

// a word with no blancs
IDENTIFICATION_VARIABLE
  : ('a' .. 'z' | 'A' .. 'Z' | '_') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
  ;

STRING_VARIABLE
  : ('a' .. 'z' | 'A' .. 'Z' | '_') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
  ;

// Whitespaces to be skipped
WS
  : [ \t\r\n] -> skip
  ;
