/*
 *  The scanner definition for COOL.
 */


%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

static int comments_stack;
static int null_char_present;
static std::string current_string;

%}

DIGIT	[0-9]
INTEGER	{DIGIT}+
ESCAPE	\\
NEWLINE	\n
NULL_CHAR \0
ONE_CHAR_TOKENS	[:+\-*/=)(}{~.,;<@]
TRUE    t(?i:rue)
FALSE	f(?i:alse)
LPAREN	\(
RPAREN	\)
STAR	\*
ALPHANUM  [a-zA-Z0-9_]
TYPE_ID	  [A-Z]{ALPHANUM}*
OBJECT_ID [a-z]{ALPHANUM}*
QUOTE	  \"
HYPHEN	  -
WHITESPACE  [ \t\r\f\v]

DARROW          =>

%x COMMENTS COMMENT_IN_LINE STRING

%%

(?i:class)	return CLASS;
(?i:else)	return ELSE;
(?i:fi)		return FI;
(?i:if)		return IF;
(?i:in)		return IN;
(?i:inherits)	return INHERITS;
(?i:let)	return LET;
(?i:loop)	return LOOP;
(?i:pool)	return POOL;
(?i:then)	return THEN;
(?i:while)	return WHILE;
(?i:case)	return CASE;
(?i:esac)	return ESAC;
(?i:of)		return OF;
(?i:new)	return NEW;
(?i:isvoid)	return ISVOID;
(?i:not)	return NOT;
"<="		return LE;
"<-"		return ASSIGN;


{WHITESPACE}	;

{HYPHEN}{HYPHEN} {
    BEGIN COMMENT_IN_LINE;
}

{DARROW}		{ return (DARROW); }

{ONE_CHAR_TOKENS} {
    return int(yytext[0]);
}

{NEWLINE}	curr_lineno++;

{TRUE}	{
    cool_yylval.boolean = 1;
    return BOOL_CONST;
}

{FALSE} {
    cool_yylval.boolean = 0;
    return BOOL_CONST;
}

{INTEGER} {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}

{TYPE_ID} {
    cool_yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
}

{OBJECT_ID} {
    cool_yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
}

{STAR}{RPAREN} {
    cool_yylval.error_msg = "Mismatched *)";
    return ERROR;
}

{LPAREN}{STAR} {
    comments_stack++;
    BEGIN COMMENTS;
}

{QUOTE}	{
    BEGIN STRING;
    current_string = "";
    null_char_present = 0;
}

. {
    cool_yylval.error_msg = yytext;
    return ERROR;
}



<COMMENT_IN_LINE>{NEWLINE} {
    curr_lineno++;
    BEGIN INITIAL;
}

<COMMENT_IN_LINE><<EOF>> {
    BEGIN INITIAL;
}

<COMMENT_IN_LINE>.	;



<STRING>{QUOTE}	{
    BEGIN INITIAL;
    if (current_string.size() >= MAX_STR_CONST) {
        cool_yylval.error_msg = "String constant too long";
	return ERROR;
    }
    if (null_char_present) {
       cool_yylval.error_msg = "String contains null character";
       return ERROR;
    }
    cool_yylval.symbol = stringtable.add_string((char *)current_string.c_str());
    return STR_CONST;
}

<STRING>{ESCAPE}{NEWLINE} {
    current_string += '\n';
}

<STRING>{NEWLINE} {
    BEGIN INITIAL;
    curr_lineno++;
    cool_yylval.error_msg = "Unterminated string constant";
    return ERROR;
}

<STRING>{NULL_CHAR} {
    null_char_present = 1;
}

<STRING>{ESCAPE}. {
    char ch;
    switch((ch = yytext[1])) {
        case 'b':
	    current_string += '\b';
	    break;
	case 't':
	    current_string += '\t';
	    break;
	case 'n':
	    current_string += '\n';
	    break;
	case 'f':
	    current_string += '\f';
	    break;
	case '\0':
	    null_char_present = 1;
	    break;
	default:
	    current_string += ch;
            break;
    }
}

<STRING><<EOF>> {
    BEGIN INITIAL;
    cool_yylval.error_msg = "EOF in string";
    return ERROR;
}

<STRING>. {
    current_string += yytext;
}



<COMMENTS>{LPAREN}{STAR} {
    comments_stack++;
}

<COMMENTS>{STAR}{RPAREN} {
    comments_stack--;
    if (comments_stack == 0) {
       BEGIN INITIAL;
    }
}

<COMMENTS>{NEWLINE} {
    curr_lineno++;
}

<COMMENTS><<EOF>> {
    BEGIN INITIAL;
    cool_yylval.error_msg = "EOF in comment";
    return ERROR;
}

<COMMENTS>.	;

%%
