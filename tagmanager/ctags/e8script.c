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

#include <stdarg.h>

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
	
	if (intoken(dbp[len]))
		return FALSE;
	
	char local[80];
	strncpy(local, dbp, len);
	local[len] = 0;
	utf_lowercase(local);

	int r = strcmp(local, word);
	if (r == 0) {
		dbp += len;
		return TRUE;
	}
	return FALSE;
}

static boolean try_words(const char *word, ...)
{
	va_list words;
	va_start(words, word);
	
	const char *next_word = word;
	
	boolean result = FALSE;
	
	while (next_word) {
		result = try_word(next_word);
		if (result)
			break;
			
		next_word = va_arg(words, const char *);
	}
	
	va_end(words);
	
	return result;
}

enum __parse_state {Default, VarList, VarDone, SubName, SubDone, InSub};

static void findE8ScriptTags (void)
{
	
    vString *name = vStringNew ();
    tagEntryInfo sub, var;
    char *arglist = NULL;
    E8Script_Kind kind = K_FUNCTION;
	
				/* each of these flags is TRUE iff: */
    boolean incomment = FALSE;	/* point is inside a comment */
    boolean inquote = FALSE;	/* point is inside '..' string */
    boolean inparms = FALSE;	/* point is within parameter-list */
	enum __parse_state state = Default;
				   
	boolean go_new_line = FALSE;
	
	char *in_func = NULL;

    dbp = fileReadLine ();
    while (dbp != NULL) {
	
		int c = *dbp;
		
		if (c == '\0' || go_new_line) {
			dbp = fileReadLine();
			continue;
		}
		
		while (isspace(*dbp))
			++dbp;
			
		if (*dbp == ';') {
			
			if (in_func)
				state = InSub;
			else
				state = Default;
				
			++dbp;
			
			continue;
		}
		
		if (state == Default) {
			
			if (try_words("var", "перем", NULL)) {
				state = VarList;
				++dbp;
			} else
			if (try_words("функция", "процедура", "function", "procedure", NULL)) {
				state = SubName;
				++dbp;
			} else
				++dbp;
			
		} else
		if (state == InSub) {
		
			if (try_words("var", "перем", NULL)) {
				state = VarList;
				++dbp;
			} else
			if (try_words("конецфункции", "конецпроцедуры", "endfunction", "endprocedure", NULL)) {
				state = Default;
				/* free(in_func); */
				makeE8ScriptTag(&sub);
				in_func = NULL;				
				++dbp;
			} else
				++dbp;
				
		} else
		if (state == SubName) {
			if (starttoken(*dbp)) {
				
				const unsigned char *cp;
			
				for (cp = dbp  ;  *cp != '\0' && !endtoken (*cp)  ;  cp++)
					continue;
					
				vStringNCopyS (name, (const char*) dbp,  cp - dbp);
					
				createE8ScriptTag (&sub, name, K_FUNCTION, NULL, NULL);
				dbp = cp;		/* set dbp to e-o-token */
					
				state = InSub;
				in_func = vStringValue(name);
				
				name = vStringNew ();
					
			} else
				++dbp;
		} else
		if (state == VarList) {
			
			if (starttoken(*dbp)) {
				
				const unsigned char *cp;
			
				for (cp = dbp  ;  *cp != '\0' && !endtoken (*cp)  ;  cp++)
					continue;
					
				vStringNCopyS (name, (const char*) dbp,  cp - dbp);
				dbp = cp;		/* set dbp to e-o-token */
					
				createE8ScriptTag (&var, name, K_VARIABLE, NULL, NULL);
				
				if (in_func) {
					var.extensionFields.scope[0] = strdup("function");
					var.extensionFields.scope[1] = strdup(in_func);
				}
				
				makeE8ScriptTag (&var);
				
				state = VarDone;
				
				name = vStringNew ();
			}
			else
				++dbp;
			
		} else
		if (state == VarDone) {
		
			if (*dbp == ',')
				state = VarList;
			++dbp;
		} else
			++dbp;
	
    }  /* while not eof */
    if (arglist != NULL)
		eFree(arglist);
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
