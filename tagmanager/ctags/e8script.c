/*
*   Copyright (c) 2014, Sergey Batanov
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
* 	Tag parser for E8::Script & EvilBeaver OneScript
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "main.h"
#include "vstring.h"

#include <stdio.h>

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_FUNCTION, K_VARIABLE
} E8Script_Kind;

static kindOption E8Script_Kinds [] = {
    { TRUE, 'f', "function", "functions"},
	{ TRUE, 'v', "variable", "variables"}
};

/*
*   FUNCTION DEFINITIONS
*/

static void createE8ScriptTag (tagEntryInfo* const tag,
			     const vString* const name, const int kind,
			     const char *arglist, const char *vartype)
{
    if (E8Script_Kinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
    {
        initTagEntry (tag, vStringValue (name));

        tag->kindName = E8Script_Kinds [kind].name;
        tag->kind     = E8Script_Kinds [kind].letter;
        tag->extensionFields.arglist = arglist;
        tag->extensionFields.varType = vartype;
    }
    else
        initTagEntry (tag, NULL);
}

static void makeE8ScriptTag (const tagEntryInfo* const tag)
{
    if (tag->name != NULL)
	makeTagEntry (tag);
}

static const unsigned char* dbp;

#define starttoken(c) (isalpha ((int) c) || (int) c == '_')
#define intoken(c)    (isalnum ((int) c) || (int) c == '_' || (int) c == '.' || ((int)c >= 0x80))
#define endtoken(c)   (! intoken (c)  &&  ! isdigit ((int) c))

static boolean tail (const char *cp)
{
    boolean result = FALSE;
    register int len = 0;

    while (*cp != '\0' && tolower ((int) *cp) == tolower ((int) dbp [len]))
	cp++, len++;
    if (*cp == '\0' && !intoken (dbp [len]))
    {
	dbp += len;
	result = TRUE;
    }
    return result;
}

static int bukvaLower(int ch)
{
	if (ch >= 0x0410 && ch < 0x0430)
		return ch + 0x20;
	return ch;
}

static void utf_lowercase(char *s)
{
	unsigned char *c = (unsigned char *)s;
	while (*c) {
		if (*c < 0x80) {
			*c = tolower(*c);
		} else {
			unsigned cp = *c;
            ++c;
            cp = ((cp << 6) & 0x7ff) + ((*c) & 0x3f);
			--c;
			cp = bukvaLower(cp);

			*c = ((cp >> 6) & 0x1F) | 0xC0;
            ++c;

            *c = (cp & 0x3F) | 0x80;
		}
		++c;
	}
}

static boolean try_word(const char *word)
{
	int len = strlen(word);
	char *local = malloc(len + 1);
	strncpy(local, dbp, len);
	local[len] = 0;
	utf_lowercase(local);
	

	int r = strcmp(local, word);
	/*
	free(local); TODO: На этом месте вылетает  (?)
	*/
	if (r == 0) {
		dbp += len;
		return TRUE;
	}
	return FALSE;
}

static void findE8ScriptTags (void)
{
	
    vString *name = vStringNew ();
    tagEntryInfo tag;
    char *arglist = NULL;
    char *vartype = NULL;
    E8Script_Kind kind = K_FUNCTION;
				/* each of these flags is TRUE iff: */
    boolean incomment = FALSE;	/* point is inside a comment */
    int comment_char = '\0';    /* type of current comment */
    boolean inquote = FALSE;	/* point is inside '..' string */
    boolean get_tagname = FALSE;/* point is after PROCEDURE/FUNCTION
				    keyword, so next item = potential tag */
    boolean found_tag = FALSE;	/* point is after a potential tag */
    boolean inparms = FALSE;	/* point is within parameter-list */
    boolean verify_tag = FALSE;	/* point has passed the parm-list, so the
				   next token will determine whether this
				   is a FORWARD/EXTERN to be ignored, or
				   whether it is a real tag */
				   
	boolean go_new_line = FALSE;

    dbp = fileReadLine ();
    while (dbp != NULL) {
		int c = *dbp++;

		if (c == '\0' || go_new_line)		/* if end of line */
		{
			dbp = fileReadLine ();
			go_new_line = FALSE;
			if (dbp == NULL  ||  *dbp == '\0')
				continue;
			if (!((found_tag && verify_tag) || get_tagname))
				c = *dbp++;		/* only if don't need *dbp pointing
								to the beginning of the name of
								the procedure or function */
		}
		if (incomment) {
			if (comment_char == '/' && c == '*' && *dbp == '/') {
				dbp++;
				incomment = FALSE;
			}
			continue;
		}
		else if (inquote) {
			if (c == '\"')
				inquote = FALSE;
			continue;
		}
		else switch (c) {
			case '\"':
					inquote = TRUE;	/* found first quote */
					continue;
			case '/':
					if (*dbp == '/') {
						go_new_line = TRUE;
						continue;
					}
					if (*dbp == '*') {	/* found open (* comment */
						incomment = TRUE;
						comment_char = c;
						dbp++;
					}
					break;
			case '(':
					if (found_tag)  /* found '(' after tag, i.e., parm-list */
						inparms = TRUE;
					continue;
			case ')':		/* end of parms list */
					if (inparms)
						inparms = FALSE;
					continue;
		}
		if (found_tag /*&& verify_tag*/ && *dbp != ' ') {
			if (*dbp == '\0')
				continue;
			if (found_tag /*&& verify_tag*/) { /* not external proc, so make tag */
				found_tag = FALSE;
				verify_tag = FALSE;
				makeE8ScriptTag (&tag);
				continue;
			}
		}
		if (get_tagname)		/* grab name of proc or fn */
		{
			const unsigned char *cp;

			if (*dbp == '\0')
				continue;

			/* grab block name */
			while (isspace ((int) *dbp))
				++dbp;
				
			for (cp = dbp  ;  *cp != '\0' && !endtoken (*cp)  ;  cp++)
				continue;
				
			vStringNCopyS (name, (const char*) dbp,  cp - dbp);
			if (arglist != NULL)
				eFree(arglist);
				
			if (kind == K_FUNCTION && vartype != NULL)
				eFree(vartype);
				
			createE8ScriptTag (&tag, name, kind, arglist, (kind == K_FUNCTION) ? vartype : NULL);
			dbp = cp;		/* set dbp to e-o-token */
			get_tagname = FALSE;
			found_tag = TRUE;
			/* and proceed to check for "extern" */
		}
		else if (!incomment && !inquote && !found_tag) {
			--dbp;
			if (try_word("procedure")) {
				get_tagname = TRUE;
				kind = K_FUNCTION;
			} else
			if (try_word("function")) {
				get_tagname = TRUE;
				kind = K_FUNCTION;
			} else
			if (try_word("процедура")) {
				get_tagname = TRUE;
				kind = K_FUNCTION;
			} else
			if (try_word("функция")) {
				get_tagname = TRUE;
				kind = K_FUNCTION;
			} else
				++dbp;
		}
    }  /* while not eof */
    if (arglist != NULL)
		eFree(arglist);
    if (vartype != NULL)
		eFree(vartype);
    vStringDelete(name);
}

extern parserDefinition* E8ScriptParser (void)
{
    static const char *const extensions [] = { "e8s", "os", "1scr", NULL };
    parserDefinition* def = parserNew ("E8Script");
    def->extensions = extensions;
    def->kinds      = E8Script_Kinds;
    def->kindCount  = KIND_COUNT (E8Script_Kinds);
    def->parser     = findE8ScriptTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
