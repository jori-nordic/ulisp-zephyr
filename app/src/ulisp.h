/* uLisp ARM Release 4.4b - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 3rd April 2023

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

#define PROGMEM
#define PSTR(x) x
#define PGM_P const char *
enum pinModes {
	INPUT,
	OUTPUT,
	INPUT_PULLUP,
	INPUT_PULLDOWN,
};

#define LOW 0UL
#define HIGH 1UL
#define LED_BUILTIN 0

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <zephyr/kernel.h>
#include <math.h>

// Lisp Library

// Compile options

// C Macros

#define nil    NULL
#define car(x) (((object *)(x))->car)
#define cdr(x) (((object *)(x))->cdr)

#define first(x)  (((object *)(x))->car)
#define second(x) (car(cdr(x)))
#define cddr(x)   (cdr(cdr(x)))
#define third(x)  (car(cdr(cdr(x))))

#define push(x, y) ((y) = cons((x), (y)))
#define pop(y)     ((y) = cdr(y))

#define integerp(x)   ((x) != NULL && (x)->type == NUMBER)
#define floatp(x)     ((x) != NULL && (x)->type == FLOAT)
#define symbolp(x)    ((x) != NULL && (x)->type == SYMBOL)
#define stringp(x)    ((x) != NULL && (x)->type == STRING)
#define characterp(x) ((x) != NULL && (x)->type == CHARACTER)
#define arrayp(x)     ((x) != NULL && (x)->type == ARRAY)
#define streamp(x)    ((x) != NULL && (x)->type == STREAM)

#define mark(x)   (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x) (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x) ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT   1

#define setflag(x) (Flags = Flags | 1 << (x))
#define clrflag(x) (Flags = Flags & ~(1 << (x)))
#define tstflag(x) (Flags & 1 << (x))

#define issp(x)        (x == ' ' || x == '\n' || x == '\r' || x == '\t')
#define isbr(x)        (x == ')' || x == '(' || x == '"' || x == '#')
#define longsymbolp(x) (((x)->name & 0x03) == 0)
#define twist(x)       ((uint32_t)((x) << 2) | (((x)&0xC0000000) >> 30))
#define untwist(x)     (((x) >> 2 & 0x3FFFFFFF) | ((x)&0x03) << 30)
#define arraysize(x)   (sizeof(x) / sizeof(x[0]))
#define PACKEDS        0x43238000
#define BUILTINS       0xF4240000
#define ENDFUNCTIONS   1536

// Code marker stores start and end of code block
#define startblock(x) ((x->integer) & 0xFFFF)
#define endblock(x)   ((x->integer) >> 16 & 0xFFFF)

// Constants

enum type {
	ZZERO = 0,
	SYMBOL = 2,
	CODE = 4,
	NUMBER = 6,
	STREAM = 8,
	CHARACTER = 10,
	FLOAT = 12,
	ARRAY = 14,
	STRING = 16,
	PAIR = 18
}; // ARRAY STRING and PAIR must be last
enum token {
	UNUSED,
	BRA,
	KET,
	QUO,
	DOT
};
enum stream {
	SERIALSTREAM,
	I2CSTREAM,
	SPISTREAM,
	SDSTREAM,
	WIFISTREAM,
	STRINGSTREAM,
	GFXSTREAM
};
enum fntypes_t {
	OTHER_FORMS,
	TAIL_FORMS,
	FUNCTIONS,
	SPECIAL_FORMS
};

// Typedefs

typedef void* TwoWire;
typedef void* File;
typedef bool boolean;

typedef uint32_t symbol_t;

struct sobject;

typedef struct sobject {
	union {
		struct {
			struct sobject *car;
			struct sobject *cdr;
		};
		struct {
			unsigned int type;
			union {
				symbol_t name;
				int integer;
				int chars; // For strings
				float single_float;
			};
		};
	};
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef const struct {
	const char *string;
	fn_ptr_type fptr;
	uint8_t minmax;
	const char *doc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);

typedef uint16_t builtin_t;

enum builtins {
	NIL,
	TEE,
	NOTHING,
	OPTIONAL,
	INITIALELEMENT,
	ELEMENTTYPE,
	BIT,
	AMPREST,
	LAMBDA,
	LET,
	LETSTAR,
	CLOSURE,
	PSTAR,
	QUOTE,
	DEFUN,
	DEFVAR,
	DEFCODE,
	CAR,
	FIRST,
	CDR,
	REST,
	NTH,
	AREF,
	STRINGFN,
	PINMODE,
	DIGITALWRITE,
	ANALOGREAD,
	ANALOGREFERENCE,
	REGISTER,
	FORMAT,
};

// Flags
enum flag {
	PRINTREADABLY,
	RETURNFLAG,
	ESCAPE,
	EXITEDITOR,
	LIBRARYLOADED,
	NOESC,
	NOECHO,
	MUFFLEERRORS
};

// Forward references
void pfstring(PGM_P s, pfun_t pfun);

// Error handling

/*
  errorsub - used by all the error routines.
  Prints: "Error: 'fname' string", where fname is the name of the Lisp function in which the error
  occurred.
*/
void errorsub(symbol_t fname, PGM_P string);

void errorend();

/*
  errorsym - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string: symbol", where fname is the name of the user Lisp function in
  which the error occurred, and symbol is the object generating the error.
*/
void errorsym(symbol_t fname, PGM_P string, object *symbol);

/*
  errorsym2 - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string", where fname is the name of the user Lisp function in which the
  error occurred.
*/
void errorsym2(symbol_t fname, PGM_P string);

/*
  error - prints an error message and reenters the REPL.
  Prints: "Error: 'Context' string: symbol", where Context is the name of the built-in Lisp function
  in which the error occurred, and symbol is the object generating the error.
*/
void error(PGM_P string, object *symbol);

/*
  error2 - prints an error message and reenters the REPL.
  Prints: "Error: 'Context' string", where Context is the name of the built-in Lisp function in
  which the error occurred.
*/
void error2(PGM_P string);

/*
  formaterr - displays a format error with a ^ pointing to the error
*/
void formaterr(object *formatstr, PGM_P string, uint8_t p);

// Set up workspace

/*
  initworkspace - initialises the workspace into a linked list of free objects
*/
void initworkspace();

/*
  myalloc - returns the first object from the linked list of free objects
*/
object *myalloc();

/*
  myfree - adds obj to the linked list of free objects.
  inline makes gc significantly faster
*/
inline void myfree(object *obj);

// Make each type of object

/*
  number - make an integer object with value n and return it
*/
object *number(int n);

/*
  makefloat - make a floating point object with value f and return it
*/
object *makefloat(float f);

/*
  character - make a character object with value c and return it
*/
object *character(uint8_t c);

/*
  cons - make a cons with arg1 and arg2 return it
*/
object *cons(object *arg1, object *arg2);

/*
  symbol - make a symbol object with value name and return it
*/
object *symbol(symbol_t name);

/*
  bsymbol - make a built-in symbol
*/
inline object *bsymbol(builtin_t name);

/*
  codehead - make a code header object with value entry and return it
*/
object *codehead(int entry);

/*
  intern - looks through the workspace for an existing occurrence of symbol name and returns it,
  otherwise calls symbol(name) to create a new symbol.
*/
object *intern(symbol_t name);

/*
  eqsymbols - compares the long string/symbol obj with the string in buffer.
*/
bool eqsymbols(object *obj, char *buffer);

/*
  internlong - looks through the workspace for an existing occurrence of the long symbol in buffer
  and returns it, otherwise calls lispstring(buffer) to create a new symbol.
*/
object *internlong(char *buffer);

/*
  stream - makes a stream object defined by streamtype and address, and returns it
*/
object *stream(uint8_t streamtype, uint8_t address);

/*
  newstring - makes an empty string object and returns it
*/
object *newstring();

// Garbage collection

/*
  markobject - recursively marks reachable objects, starting from obj
*/
void markobject(object *obj);

/*
  sweep - goes through the workspace freeing objects that have not been marked,
  and unmarks marked objects
*/
void sweep();

/*
  gc - performs garbage collection by calling markobject() on each of the pointers to objects in
  use, followed by sweep() to free unused objects.
*/
void gc(object *form, object *env);

// Compact image

/*
  movepointer - corrects pointers to an object that has moved from 'from' to 'to'
*/
void movepointer(object *from, object *to);

/*
  compactimage - compacts the image by moving objects to the lowest possible position in the
  workspace
*/
uintptr_t compactimage(object **arg);

// Make SD card filename

char *MakeFilename(object *arg, char *buffer);

// Save-image and load-image

void SDWrite32(File file, int data);

int SDRead32(File file);
void FSWrite32(File file, uint32_t data);

uint32_t FSRead32(File file);
void FlashBusy();

/* inline void FlashWrite(uint8_t data); */

/* inline uint8_t FlashReadByte(); */

void FlashWriteByte(uint32_t *addr, uint8_t data);

void FlashWriteEnable();

bool FlashCheck();

void FlashBeginWrite(uint32_t *addr, uint32_t bytes);

void FlashWrite32(uint32_t *addr, uint32_t data);

/* inline void FlashEndWrite(uint32_t *addr); */

void FlashBeginRead(uint32_t *addr);

uint32_t FlashRead32(uint32_t *addr);

/* inline void FlashEndRead(uint32_t *addr); */

void row_erase(const volatile void *addr);

void page_clear();

void page_write();

int saveimage(object *arg);

int loadimage(object *arg);

void autorunimage();

// Tracing

/*
  tracing - returns a number between 1 and TRACEMAX if name is being traced, or 0 otherwise
*/
int tracing(symbol_t name);

/*
  trace - enables tracing of symbol name and adds it to the array TraceFn[].
*/
void trace(symbol_t name);

/*
  untrace - disables tracing of symbol name and removes it from the array TraceFn[].
*/
void untrace(symbol_t name);

// Helper functions

/*
  consp - implements Lisp consp
*/
bool consp(object *x);

/*
  atom - implements Lisp atom
*/
#define atom(x) (!consp(x))

/*
  listp - implements Lisp listp
*/
bool listp(object *x);

/*
  improperp - tests whether x is an improper list
*/
#define improperp(x) (!listp(x))

object *quote(object *arg);

// Radix 40 encoding

/*
  builtin - converts a symbol name to builtin
*/
builtin_t builtin(symbol_t name);

/*
 sym - converts a builtin to a symbol name
*/
symbol_t sym(builtin_t x);

/*
  toradix40 - returns a number from 0 to 39 if the character can be encoded, or -1 otherwise.
*/
int8_t toradix40(char ch);

/*
  fromradix40 - returns the character encoded by the number n.
*/
char fromradix40(char n);

/*
  pack40 - packs six radix40-encoded characters from buffer into a 32-bit number and returns it.
*/
uint32_t pack40(char *buffer);

/*
  valid40 - returns true if the symbol in buffer can be encoded as six radix40-encoded characters.
*/
bool valid40(char *buffer);

/*
  digitvalue - returns the numerical value of a hexadecimal digit, or 16 if invalid.
*/
int8_t digitvalue(char d);

/*
  checkinteger - check that obj is an integer and return it
*/
int checkinteger(object *obj);

/*
  checkbitvalue - check that obj is an integer equal to 0 or 1 and return it
*/
int checkbitvalue(object *obj);

/*
  checkintfloat - check that obj is an integer or floating-point number and return the number
*/
float checkintfloat(object *obj);

/*
  checkchar - check that obj is a character and return the character
*/
int checkchar(object *obj);

/*
  checkstring - check that obj is a string
*/
object *checkstring(object *obj);

int isstream(object *obj);

int isbuiltin(object *obj, builtin_t n);

bool builtinp(symbol_t name);

int checkkeyword(object *obj);

/*
  checkargs - checks that the number of objects in the list args
  is within the range specified in the symbol lookup table
*/
void checkargs(object *args);

/*
  eq - implements Lisp eq
*/
boolean eq(object *arg1, object *arg2);

/*
  equal - implements Lisp equal
*/
boolean equal(object *arg1, object *arg2);

/*
  listlength - returns the length of a list
*/
int listlength(object *list);

/*
  checkarguments - checks the arguments list in a special form such as with-xxx,
  dolist, or dotimes.
*/
object *checkarguments(object *args, int min, int max);

// Mathematical helper functions

/*
  add_floats - used by fn_add
  Converts the numbers in args to floats, adds them to fresult, and returns the sum as a Lisp float.
*/
object *add_floats(object *args, float fresult);

/*
  subtract_floats - used by fn_subtract with more than one argument
  Converts the numbers in args to floats, subtracts them from fresult, and returns the result as a
  Lisp float.
*/
object *subtract_floats(object *args, float fresult);

/*
  negate - used by fn_subtract with one argument
  If the result is an integer, and negating it doesn't overflow, keep the result as an integer.
  Otherwise convert the result to a float, negate it, and return the result as a Lisp float.
*/
object *negate(object *arg);

/*
  multiply_floats - used by fn_multiply
  Converts the numbers in args to floats, adds them to fresult, and returns the result as a Lisp
  float.
*/
object *multiply_floats(object *args, float fresult);

/*
  divide_floats - used by fn_divide
  Converts the numbers in args to floats, divides fresult by them, and returns the result as a Lisp
  float.
*/
object *divide_floats(object *args, float fresult);

/*
  myround - rounds
  Returns t if the argument is a floating-point number.
*/
int myround(float number);

/*
  compare - a generic compare function
  Used to implement the other comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
object *compare(object *args, bool lt, bool gt, bool eq);

/*
  intpower - calculates base to the power exp as an integer
*/
int intpower(int base, int exp);

// Association lists

/*
  assoc - looks for key in an association list and returns the matching pair, or nil if not found
*/
object *assoc(object *key, object *list);

/*
  delassoc - deletes the pair matching key from an association list and returns the key, or nil if
  not found
*/
object *delassoc(object *key, object **alist);

// Array utilities

/*
  nextpower2 - returns the smallest power of 2 that is equal to or greater than n
*/
int nextpower2(int n);

/*
  buildarray - builds an array with n elements using a tree of size s which must be a power of 2
  The elements are initialised to the default def
*/
object *buildarray(int n, int s, object *def);

object *makearray(object *dims, object *def, bool bitp);

/*
  arrayref - returns a pointer to the element specified by index in the array of size s
*/
object **arrayref(object *array, int index, int size);

/*
  getarray - gets a pointer to an element in a multi-dimensional array, given a list of the
  subscripts subs If the first subscript is negative it's a bit array and bit is set to the bit
  number
*/
object **getarray(object *array, object *subs, object *env, int *bit);

/*
  rslice - reads a slice of an array recursively
*/
void rslice(object *array, int size, int slice, object *dims, object *args);

/*
  readarray - reads a list structure from args and converts it to a d-dimensional array.
  Uses rslice for each of the slices of the array.
*/
object *readarray(int d, object *args);

/*
  readbitarray - reads an item in the format #*1010101000110 by reading it and returning a list of
  integers, and then converting that to a bit array
*/
object *readbitarray(gfun_t gfun);

/*
  pslice - prints a slice of an array recursively
*/
void pslice(object *array, int size, int slice, object *dims, pfun_t pfun, bool bitp);

/*
  printarray - prints an array in the appropriate Lisp format
*/
void printarray(object *array, pfun_t pfun);

// String utilities

void indent(uint8_t spaces, char ch, pfun_t pfun);

/*
  startstring - starts building a string
*/
object *startstring();

/*
  princtostring - implements Lisp princtostring function
*/
object *princtostring(object *arg);

/*
  buildstring - adds a character on the end of a string
  Handles Lisp strings packed four characters per 32-bit word
*/
void buildstring(char ch, object **tail);

/*
  copystring - returns a copy of a Lisp string
*/
object *copystring(object *arg);

/*
  readstring - reads characters from an input stream up to delimiter delim
  and returns a Lisp string
*/
object *readstring(uint8_t delim, gfun_t gfun);

/*
  stringlength - returns the length of a Lisp string
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
int stringlength(object *form);

/*
  nthchar - returns the nth character from a Lisp string
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
uint8_t nthchar(object *string, int n);

/*
  gstr - reads a character from a string stream
*/
int gstr();

/*
  pstr - prints a character to a string stream
*/
void pstr(char c);

/*
  lispstring - converts a C string to a Lisp string
*/
object *lispstring(char *s);

/*
  stringcompare - a generic string compare function
  Used to implement the other string comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
bool stringcompare(object *args, bool lt, bool gt, bool eq);

/*
  documentation - returns the documentation string of a built-in or user-defined function.
*/
object *documentation(object *arg, object *env);

/*
  apropos - finds the user-defined and built-in functions whose names contain the specified string
  or symbol, and prints them if print is true, or returns them in a list.
*/
object *apropos(object *arg, bool print);

/*
  cstring - converts a Lisp string to a C string in buffer and returns buffer
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
char *cstring(object *form, char *buffer, int buflen);

/*
  ipstring - parses an IP address from a Lisp string and returns it as an IPAddress type (uint32_t)
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
uint32_t ipstring(object *form);

// Lookup variable in environment

object *value(symbol_t n, object *env);

/*
  findpair - returns the (var . value) pair bound to variable var in the local or global environment
*/
object *findpair(object *var, object *env);

/*
  boundp - tests whether var is bound to a value
*/
bool boundp(object *var, object *env);

/*
  findvalue - returns the value bound to variable var, or gives an error if unbound
*/
object *findvalue(object *var, object *env);

// Handling closures

object *closure(int tc, symbol_t name, object *function, object *args, object **env);

object *apply(object *function, object *args, object *env);

// In-place operations

/*
  place - returns a pointer to an object referenced in the second argument of an
  in-place operation such as setf. bit is used to indicate the bit position in a bit array
*/
object **place(object *args, object *env, int *bit);

// Checked car and cdr

/*
  carx - car with error checking
*/
object *carx(object *arg);

/*
  cdrx - cdr with error checking
*/
object *cdrx(object *arg);

/*
  cxxxr - implements a general cxxxr function,
  pattern is a sequence of bits 0b1xxx where x is 0 for a and 1 for d.
*/
object *cxxxr(object *args, uint8_t pattern);

// Mapping helper functions

/*
  mapcarfun - function specifying how to combine the results in mapcar
*/
void mapcarfun(object *result, object **tail);

/*
  mapcanfun - function specifying how to combine the results in mapcan
*/
void mapcanfun(object *result, object **tail);

/*
  mapcarcan - function used by marcar and mapcan
  It takes the arguments, the env, and a function specifying how the results are combined.
*/
object *mapcarcan(object *args, object *env, mapfun_t fun);

// I2C interface for up to two ports, using Arduino Wire

void I2Cinit(TwoWire *port, bool enablePullup);

int I2Cread(TwoWire *port);

void I2Cwrite(TwoWire *port, uint8_t data);

bool I2Cstart(TwoWire *port, uint8_t address, uint8_t read);

bool I2Crestart(TwoWire *port, uint8_t address, uint8_t read);

void I2Cstop(TwoWire *port, uint8_t read);

// Streams

int spiread (void);
int i2cread (void);

#if defined(sdcardsupport)
File SDpfile, SDgfile;
int SDread();
#endif

void serialbegin (int address, int baud);

void serialend (int address);

gfun_t gstreamfun(object *args);

inline void spiwrite(char c);
inline void i2cwrite(char c);
#if defined(sdcardsupport)
inline void SDwrite(char c);
#endif
#if defined(gfxsupport)
inline void gfxwrite(char c);
#endif

pfun_t pstreamfun(object *args);

// Check pins - these are board-specific not processor-specific

void checkanalogread(int pin);

void checkanalogwrite(int pin);

// Note
;
void tone(uint32_t pin, uint32_t frequency);

void noTone(uint32_t pin);

void playnote(int pin, int note, int octave);

void nonote(int pin);

// Sleep

void initsleep();

void doze(int secs);

// Prettyprint

void pcount(char c);

/*
  atomwidth - calculates the character width of an atom
*/
uint8_t atomwidth(object *obj);

uint8_t basewidth(object *obj, uint8_t base);

bool quoted(object *obj);

int subwidth(object *obj, int w);

int subwidthlist(object *form, int w);

/*
  superprint - the main pretty-print subroutine
*/
void superprint(object *form, int lm, pfun_t pfun);

/*
  supersub - subroutine used by pprint
*/
void supersub(object *form, int lm, int super, pfun_t pfun);

/*
  edit - the Lisp tree editor
  Steps through a function definition, editing it a bit at a time, using single-key editing
  commands.
*/
object *edit(object *fun);

// Assembler

object *call(int entry, int nargs, object *args, object *env);

void putcode(object *arg, int origin, int pc);

int assemble(int pass, int origin, object *entries, object *env, object *pcpair);

// Special forms

object *sp_quote(object *args, object *env);

/*
  (or item*)
  Evaluates its arguments until one returns non-nil, and returns its value.
*/
object *sp_or(object *args, object *env);

/*
  (defun name (parameters) form*)
  Defines a function.
*/
object *sp_defun(object *args, object *env);

/*
  (defvar variable form)
  Defines a global variable.
*/
object *sp_defvar(object *args, object *env);

/*
  (setq symbol value [symbol value]*)
  For each pair of arguments assigns the value of the second argument
  to the variable specified in the first argument.
*/
object *sp_setq(object *args, object *env);

/*
  (loop forms*)
  Executes its arguments repeatedly until one of the arguments calls (return),
  which then causes an exit from the loop.
*/
object *sp_loop(object *args, object *env);

/*
  (return [value])
  Exits from a (dotimes ...), (dolist ...), or (loop ...) loop construct and returns value.
*/
object *sp_return(object *args, object *env);

/*
  (push item place)
  Modifies the value of place, which should be a list, to add item onto the front of the list,
  and returns the new list.
*/
object *sp_push(object *args, object *env);

/*
  (pop place)
  Modifies the value of place, which should be a list, to remove its first item, and returns that
  item.
*/
object *sp_pop(object *args, object *env);

// Accessors

/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
*/
object *sp_incf(object *args, object *env);

/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
*/
object *sp_decf(object *args, object *env);

/*
  (setf place value [place value]*)
  For each pair of arguments modifies a place to the result of evaluating value.
*/
object *sp_setf(object *args, object *env);

// Other special forms

/*
  (dolist (var list [result]) form*)
  Sets the local variable var to each element of list in turn, and executes the forms.
  It then returns result, or nil if result is omitted.
*/
object *sp_dolist(object *args, object *env);

/*
  (dotimes (var number [result]) form*)
  Executes the forms number times, with the local variable var set to each integer from 0 to
  number-1 in turn. It then returns result, or nil if result is omitted.
*/
object *sp_dotimes(object *args, object *env);

/*
  (trace [function]*)
  Turns on tracing of up to TRACEMAX user-defined functions,
  and returns a list of the functions currently being traced.
*/
object *sp_trace(object *args, object *env);

/*
  (untrace [function]*)
  Turns off tracing of up to TRACEMAX user-defined functions, and returns a list of the functions
  untraced. If no functions are specified it untraces all functions.
*/
object *sp_untrace(object *args, object *env);

/*
  (for-millis ([number]) form*)
  Executes the forms and then waits until a total of number milliseconds have elapsed.
  Returns the total number of milliseconds taken.
*/
object *sp_formillis(object *args, object *env);

/*
  (time form)
  Prints the value returned by the form, and the time taken to evaluate the form
  in milliseconds or seconds.
*/
object *sp_time(object *args, object *env);

/*
  (with-output-to-string (str) form*)
  Returns a string containing the output to the stream variable str.
*/
object *sp_withoutputtostring(object *args, object *env);

/*
  (with-serial (str port [baud]) form*)
  Evaluates the forms with str bound to a serial-stream using port.
  The optional baud gives the baud rate divided by 100, default 96.
*/
object *sp_withserial(object *args, object *env);

/*
  (with-i2c (str [port] address [read-p]) form*)
  Evaluates the forms with str bound to an i2c-stream defined by address.
  If read-p is nil or omitted the stream is written to, otherwise it specifies the number of bytes
  to be read from the stream. If port is omitted it defaults to 0, otherwise it specifies the port,
  0 or 1.
*/
object *sp_withi2c(object *args, object *env);

/*
  (with-spi (str pin [clock] [bitorder] [mode] [port]) form*)
  Evaluates the forms with str bound to an spi-stream.
  The parameters specify the enable pin, clock in kHz (default 4000),
  bitorder 0 for LSBFIRST and 1 for MSBFIRST (default 1), SPI mode (default 0), and port 0 or 1
  (default 0).
*/
object *sp_withspi(object *args, object *env);

/*
  (with-sd-card (str filename [mode]) form*)
  Evaluates the forms with str bound to an sd-stream reading from or writing to the file filename.
  If mode is omitted the file is read, otherwise 0 means read, 1 write-append, or 2 write-overwrite.
*/
object *sp_withsdcard(object *args, object *env);

// Assembler

/*
  (defcode name (parameters) form*)
  Creates a machine-code function called name from a series of 16-bit integers given in the body of
  the form. These are written into RAM, and can be executed by calling the function in the same way
  as a normal Lisp function.
*/
object *sp_defcode(object *args, object *env);

// Tail-recursive forms

/*
  (progn form*)
  Evaluates several forms grouped together into a block, and returns the result of evaluating the
  last form.
*/
object *tf_progn(object *args, object *env);

/*
  (if test then [else])
  Evaluates test. If it's non-nil the form then is evaluated and returned;
  otherwise the form else is evaluated and returned.
*/
object *tf_if(object *args, object *env);

/*
  (cond ((test form*) (test form*) ... ))
  Each argument is a list consisting of a test optionally followed by one or more forms.
  If the test evaluates to non-nil the forms are evaluated, and the last value is returned as the
  result of the cond. If the test evaluates to nil, none of the forms are evaluated, and the next
  argument is processed in the same way.
*/
object *tf_cond(object *args, object *env);

/*
  (when test form*)
  Evaluates the test. If it's non-nil the forms are evaluated and the last value is returned.
*/
object *tf_when(object *args, object *env);

/*
  (unless test form*)
  Evaluates the test. If it's nil the forms are evaluated and the last value is returned.
*/
object *tf_unless(object *args, object *env);

/*
  (case keyform ((key form*) (key form*) ... ))
  Evaluates a keyform to produce a test key, and then tests this against a series of arguments,
  each of which is a list containing a key optionally followed by one or more forms.
*/
object *tf_case(object *args, object *env);

/*
  (and item*)
  Evaluates its arguments until one returns nil, and returns the last value.
*/
object *tf_and(object *args, object *env);

// Core functions

/*
  (not item)
  Returns t if its argument is nil, or nil otherwise. Equivalent to null.
*/
object *fn_not(object *args, object *env);

/*
  (cons item item)
  If the second argument is a list, cons returns a new list with item added to the front of the
  list. If the second argument isn't a list cons returns a dotted pair.
*/
object *fn_cons(object *args, object *env);

/*
  (atom item)
  Returns t if its argument is a single number, symbol, or nil.
*/
object *fn_atom(object *args, object *env);

/*
  (listp item)
  Returns t if its argument is a list.
*/
object *fn_listp(object *args, object *env);

/*
  (consp item)
  Returns t if its argument is a non-null list.
*/
object *fn_consp(object *args, object *env);

/*
  (symbolp item)
  Returns t if its argument is a symbol.
*/
object *fn_symbolp(object *args, object *env);

/*
  (arrayp item)
  Returns t if its argument is an array.
*/
object *fn_arrayp(object *args, object *env);

/*
  (boundp item)
  Returns t if its argument is a symbol with a value.
*/
object *fn_boundp(object *args, object *env);

/*
  (keywordp item)
  Returns t if its argument is a keyword.
*/
object *fn_keywordp(object *args, object *env);

/*
  (set symbol value [symbol value]*)
  For each pair of arguments, assigns the value of the second argument to the value of the first
  argument.
*/
object *fn_setfn(object *args, object *env);

/*
  (streamp item)
  Returns t if its argument is a stream.
*/
object *fn_streamp(object *args, object *env);

/*
  (eq item item)
  Tests whether the two arguments are the same symbol, same character, equal numbers,
  or point to the same cons, and returns t or nil as appropriate.
*/
object *fn_eq(object *args, object *env);

/*
  (equal item item)
  Tests whether the two arguments are the same symbol, same character, equal numbers,
  or point to the same cons, and returns t or nil as appropriate.
*/
object *fn_equal(object *args, object *env);

// List functions

/*
  (car list)
  Returns the first item in a list.
*/
object *fn_car(object *args, object *env);

/*
  (cdr list)
  Returns a list with the first item removed.
*/
object *fn_cdr(object *args, object *env);

/*
  (caar list)
*/
object *fn_caar(object *args, object *env);

/*
  (cadr list)
*/
object *fn_cadr(object *args, object *env);

/*
  (cdar list)
  Equivalent to (cdr (car list)).
*/
object *fn_cdar(object *args, object *env);

/*
  (cddr list)
  Equivalent to (cdr (cdr list)).
*/
object *fn_cddr(object *args, object *env);

/*
  (caaar list)
  Equivalent to (car (car (car list))).
*/
object *fn_caaar(object *args, object *env);

/*
  (caadr list)
  Equivalent to (car (car (cdar list))).
*/
object *fn_caadr(object *args, object *env);

/*
  (cadar list)
  Equivalent to (car (cdr (car list))).
*/
object *fn_cadar(object *args, object *env);

/*
  (caddr list)
  Equivalent to (car (cdr (cdr list))).
*/
object *fn_caddr(object *args, object *env);

/*
  (cdaar list)
  Equivalent to (cdar (car (car list))).
*/
object *fn_cdaar(object *args, object *env);

/*
  (cdadr list)
  Equivalent to (cdr (car (cdr list))).
*/
object *fn_cdadr(object *args, object *env);

/*
  (cddar list)
  Equivalent to (cdr (cdr (car list))).
*/
object *fn_cddar(object *args, object *env);

/*
  (cdddr list)
  Equivalent to (cdr (cdr (cdr list))).
*/
object *fn_cdddr(object *args, object *env);

/*
  (length item)
  Returns the number of items in a list, the length of a string, or the length of a one-dimensional
  array.
*/
object *fn_length(object *args, object *env);

/*
  (array-dimensions item)
  Returns a list of the dimensions of an array.
*/
object *fn_arraydimensions(object *args, object *env);

/*
  (list item*)
  Returns a list of the values of its arguments.
*/
object *fn_list(object *args, object *env);

/*
  (make-array size [:initial-element element] [:element-type 'bit])
  If size is an integer it creates a one-dimensional array with elements from 0 to size-1.
  If size is a list of n integers it creates an n-dimensional array with those dimensions.
  If :element-type 'bit is specified the array is a bit array.
*/
object *fn_makearray(object *args, object *env);

/*
  (reverse list)
  Returns a list with the elements of list in reverse order.
*/
object *fn_reverse(object *args, object *env);

/*
  (nth number list)
  Returns the nth item in list, counting from zero.
*/
object *fn_nth(object *args, object *env);

/*
  (aref array index [index*])
  Returns an element from the specified array.
*/
object *fn_aref(object *args, object *env);

/*
  (assoc key list)
  Looks up a key in an association list of (key . value) pairs,
  and returns the matching pair, or nil if no pair is found.
*/
object *fn_assoc(object *args, object *env);

/*
  (member item list)
  Searches for an item in a list, using eq, and returns the list starting from the first occurrence
  of the item, or nil if it is not found.
*/
object *fn_member(object *args, object *env);

/*
  (apply function list)
  Returns the result of evaluating function, with the list of arguments specified by the second
  parameter.
*/
object *fn_apply(object *args, object *env);

/*
  (funcall function argument*)
  Evaluates function with the specified arguments.
*/
object *fn_funcall(object *args, object *env);

/*
  (append list*)
  Joins its arguments, which should be lists, into a single list.
*/
object *fn_append(object *args, object *env);

/*
  (mapc function list1 [list]*)
  Applies the function to each element in one or more lists, ignoring the results.
  It returns the first list argument.
*/
object *fn_mapc(object *args, object *env);

/*
  (mapcar function list1 [list]*)
  Applies the function to each element in one or more lists, and returns the resulting list.
*/
object *fn_mapcar(object *args, object *env);

/*
  (mapcan function list1 [list]*)
  Applies the function to each element in one or more lists. The results should be lists,
  and these are appended together to give the value returned.
*/
object *fn_mapcan(object *args, object *env);

// Arithmetic functions

/*
  (+ number*)
  Adds its arguments together.
  If each argument is an integer, and the running total doesn't overflow, the result is an integer,
  otherwise a floating-point number.
*/
object *fn_add(object *args, object *env);

/*
  (- number*)
  If there is one argument, negates the argument.
  If there are two or more arguments, subtracts the second and subsequent arguments from the first
  argument. If each argument is an integer, and the running total doesn't overflow, returns the
  result as an integer, otherwise a floating-point number.
*/
object *fn_subtract(object *args, object *env);

/*
  (* number*)
  Multiplies its arguments together.
  If each argument is an integer, and the running total doesn't overflow, the result is an integer,
  otherwise it's a floating-point number.
*/
object *fn_multiply(object *args, object *env);

/*
  (/ number*)
  Divides the first argument by the second and subsequent arguments.
  If each argument is an integer, and each division produces an exact result, the result is an
  integer; otherwise it's a floating-point number.
*/
object *fn_divide(object *args, object *env);

/*
  (mod number number)
  Returns its first argument modulo the second argument.
  If both arguments are integers the result is an integer; otherwise it's a floating-point number.
*/
object *fn_mod(object *args, object *env);

/*
  (1+ number)
  Adds one to its argument and returns it.
  If the argument is an integer the result is an integer if possible;
  otherwise it's a floating-point number.
*/
object *fn_oneplus(object *args, object *env);

/*
  (1- number)
  Subtracts one from its argument and returns it.
  If the argument is an integer the result is an integer if possible;
  otherwise it's a floating-point number.
*/
object *fn_oneminus(object *args, object *env);

/*
  (abs number)
  Returns the absolute, positive value of its argument.
  If the argument is an integer the result will be returned as an integer if possible,
  otherwise a floating-point number.
*/
object *fn_abs(object *args, object *env);

/*
  (random number)
  If number is an integer returns a random number between 0 and one less than its argument.
  Otherwise returns a floating-point number between zero and number.
*/
object *fn_random(object *args, object *env);

/*
  (max number*)
  Returns the maximum of one or more arguments.
*/
object *fn_maxfn(object *args, object *env);

/*
  (min number*)
  Returns the minimum of one or more arguments.
*/
object *fn_minfn(object *args, object *env);

// Arithmetic comparisons

/*
  (/= number*)
  Returns t if none of the arguments are equal, or nil if two or more arguments are equal.
*/
object *fn_noteq(object *args, object *env);

/*
  (= number*)
  Returns t if all the arguments, which must be numbers, are numerically equal, and nil otherwise.
*/
object *fn_numeq(object *args, object *env);

/*
  (< number*)
  Returns t if each argument is less than the next argument, and nil otherwise.
*/
object *fn_less(object *args, object *env);

/*
  (<= number*)
  Returns t if each argument is less than or equal to the next argument, and nil otherwise.
*/
object *fn_lesseq(object *args, object *env);

/*
  (> number*)
  Returns t if each argument is greater than the next argument, and nil otherwise.
*/
object *fn_greater(object *args, object *env);

/*
  (>= number*)
  Returns t if each argument is greater than or equal to the next argument, and nil otherwise.
*/
object *fn_greatereq(object *args, object *env);

/*
  (plusp number)
  Returns t if the argument is greater than zero, or nil otherwise.
*/
object *fn_plusp(object *args, object *env);

/*
  (minusp number)
  Returns t if the argument is less than zero, or nil otherwise.
*/
object *fn_minusp(object *args, object *env);

/*
  (zerop number)
  Returns t if the argument is zero.
*/
object *fn_zerop(object *args, object *env);

/*
  (oddp number)
  Returns t if the integer argument is odd.
*/
object *fn_oddp(object *args, object *env);

/*
  (evenp number)
  Returns t if the integer argument is even.
*/
object *fn_evenp(object *args, object *env);

// Number functions

/*
  (integerp number)
  Returns t if the argument is an integer.
*/
object *fn_integerp(object *args, object *env);

/*
  (numberp number)
  Returns t if the argument is a number.
*/
object *fn_numberp(object *args, object *env);

// Floating-point functions

/*
  (float number)
  Returns its argument converted to a floating-point number.
*/
object *fn_floatfn(object *args, object *env);

/*
  (floatp number)
  Returns t if the argument is a floating-point number.
*/
object *fn_floatp(object *args, object *env);

/*
  (sin number)
  Returns sin(number).
*/
object *fn_sin(object *args, object *env);

/*
  (cos number)
  Returns cos(number).
*/
object *fn_cos(object *args, object *env);

/*
  (tan number)
  Returns tan(number).
*/
object *fn_tan(object *args, object *env);

/*
  (asin number)
  Returns asin(number).
*/
object *fn_asin(object *args, object *env);

/*
  (acos number)
  Returns acos(number).
*/
object *fn_acos(object *args, object *env);

/*
  (atan number1 [number2])
  Returns the arc tangent of number1/number2, in radians. If number2 is omitted it defaults to 1.
*/
object *fn_atan(object *args, object *env);

/*
  (sinh number)
  Returns sinh(number).
*/
object *fn_sinh(object *args, object *env);

/*
  (cosh number)
  Returns cosh(number).
*/
object *fn_cosh(object *args, object *env);

/*
  (tanh number)
  Returns tanh(number).
*/
object *fn_tanh(object *args, object *env);

/*
  (exp number)
  Returns exp(number).
*/
object *fn_exp(object *args, object *env);

/*
  (sqrt number)
  Returns sqrt(number).
*/
object *fn_sqrt(object *args, object *env);

/*
  (log number [base])
  Returns the logarithm of number to the specified base. If base is omitted it defaults to e.
*/
object *fn_log(object *args, object *env);

/*
  (expt number power)
  Returns number raised to the specified power.
  Returns the result as an integer if the arguments are integers and the result will be within
  range, otherwise a floating-point number.
*/
object *fn_expt(object *args, object *env);

/*
  (ceiling number [divisor])
  Returns ceil(number/divisor). If omitted, divisor is 1.
*/
object *fn_ceiling(object *args, object *env);

/*
  (floor number [divisor])
  Returns floor(number/divisor). If omitted, divisor is 1.
*/
object *fn_floor(object *args, object *env);

/*
  (truncate number [divisor])
  Returns the integer part of number/divisor. If divisor is omitted it defaults to 1.
*/
object *fn_truncate(object *args, object *env);

/*
  (round number [divisor])
  Returns the integer closest to number/divisor. If divisor is omitted it defaults to 1.
*/
object *fn_round(object *args, object *env);

// Characters

/*
  (char string n)
  Returns the nth character in a string, counting from zero.
*/
object *fn_char(object *args, object *env);

/*
  (char-code character)
  Returns the ASCII code for a character, as an integer.
*/
object *fn_charcode(object *args, object *env);

/*
  (code-char integer)
  Returns the character for the specified ASCII code.
*/
object *fn_codechar(object *args, object *env);

/*
  (characterp item)
  Returns t if the argument is a character and nil otherwise.
*/
object *fn_characterp(object *args, object *env);

// Strings

/*
  (stringp item)
  Returns t if the argument is a string and nil otherwise.
*/
object *fn_stringp(object *args, object *env);

/*
  (string= string string)
  Tests whether two strings are the same.
*/
object *fn_stringeq(object *args, object *env);

/*
  (string< string string)
  Returns t if the first string is alphabetically less than the second string, and nil otherwise.
*/
object *fn_stringless(object *args, object *env);

/*
  (string> string string)
  Returns t if the first string is alphabetically greater than the second string, and nil otherwise.
*/
object *fn_stringgreater(object *args, object *env);

/*
  (sort list test)
  Destructively sorts list according to the test function, using an insertion sort, and returns the
  sorted list.
*/
object *fn_sort(object *args, object *env);

/*
  (string item)
  Converts its argument to a string.
*/
object *fn_stringfn(object *args, object *env);

/*
  (concatenate 'string string*)
  Joins together the strings given in the second and subsequent arguments, and returns a single
  string.
*/
object *fn_concatenate(object *args, object *env);

/*
  (subseq seq start [end])
  Returns a subsequence of a list or string from item start to item end-1.
*/
object *fn_subseq(object *args, object *env);

/*
  (search pattern target)
  Returns the index of the first occurrence of pattern in target,
  which can be lists or strings, or nil if it's not found.
*/
object *fn_search(object *args, object *env);

/*
  (read-from-string string)
  Reads an atom or list from the specified string and returns it.
*/
object *fn_readfromstring(object *args, object *env);

/*
  (princ-to-string item)
  Prints its argument to a string, and returns the string.
  Characters and strings are printed without quotation marks or escape characters.
*/
object *fn_princtostring(object *args, object *env);

/*
  (prin1-to-string item [stream])
  Prints its argument to a string, and returns the string.
  Characters and strings are printed with quotation marks and escape characters,
  in a format that will be suitable for read-from-string.
*/
object *fn_prin1tostring(object *args, object *env);

// Bitwise operators

/*
  (logand [value*])
  Returns the bitwise & of the values.
*/
object *fn_logand(object *args, object *env);

/*
  (logior [value*])
  Returns the bitwise | of the values.
*/
object *fn_logior(object *args, object *env);

/*
  (logxor [value*])
  Returns the bitwise ^ of the values.
*/
object *fn_logxor(object *args, object *env);

/*
  (lognot value)
  Returns the bitwise logical NOT of the value.
*/
object *fn_lognot(object *args, object *env);

/*
  (ash value shift)
  Returns the result of bitwise shifting value by shift bits. If shift is positive, value is shifted
  to the left.
*/
object *fn_ash(object *args, object *env);

/*
  (logbitp bit value)
  Returns t if bit number bit in value is a '1', and nil if it is a '0'.
*/
object *fn_logbitp(object *args, object *env);

// System functions

/*
  (eval form*)
  Evaluates its argument an extra time.
*/
object *fn_eval(object *args, object *env);

/*
  (globals)
  Returns a list of global variables.
*/
object *fn_globals(object *args, object *env);

/*
  (locals)
  Returns an association list of local variables and their values.
*/
object *fn_locals(object *args, object *env);

/*
  (makunbound symbol)
  Removes the value of the symbol from GlobalEnv and returns the symbol.
*/
object *fn_makunbound(object *args, object *env);

/*
  (break)
  Inserts a breakpoint in the program. When evaluated prints Break! and reenters the REPL.
*/
object *fn_break(object *args, object *env);

/*
  (read [stream])
  Reads an atom or list from the serial input and returns it.
  If stream is specified the item is read from the specified stream.
*/
object *fn_read(object *args, object *env);

/*
  (prin1 item [stream])
  Prints its argument, and returns its value.
  Strings are printed with quotation marks and escape characters.
*/
object *fn_prin1(object *args, object *env);

/*
  (print item [stream])
  Prints its argument with quotation marks and escape characters, on a new line, and followed by a
  space. If stream is specified the argument is printed to the specified stream.
*/
object *fn_print(object *args, object *env);

/*
  (princ item [stream])
  Prints its argument, and returns its value.
  Characters and strings are printed without quotation marks or escape characters.
*/
object *fn_princ(object *args, object *env);

/*
  (terpri [stream])
  Prints a new line, and returns nil.
  If stream is specified the new line is written to the specified stream.
*/
object *fn_terpri(object *args, object *env);

/*
  (read-byte stream)
  Reads a byte from a stream and returns it.
*/
object *fn_readbyte(object *args, object *env);

/*
  (read-line [stream])
  Reads characters from the serial input up to a newline character, and returns them as a string,
  excluding the newline. If stream is specified the line is read from the specified stream.
*/
object *fn_readline(object *args, object *env);

/*
  (write-byte number [stream])
  Writes a byte to a stream.
*/
object *fn_writebyte(object *args, object *env);

/*
  (write-string string [stream])
  Writes a string. If stream is specified the string is written to the stream.
*/
object *fn_writestring(object *args, object *env);

/*
  (write-line string [stream])
  Writes a string terminated by a newline character. If stream is specified the string is written to
  the stream.
*/
object *fn_writeline(object *args, object *env);

/*
  (restart-i2c stream [read-p])
  Restarts an i2c-stream.
  If read-p is nil or omitted the stream is written to.
  If read-p is an integer it specifies the number of bytes to be read from the stream.
*/
object *fn_restarti2c(object *args, object *env);

/*
  (gc)
  Forces a garbage collection and prints the number of objects collected, and the time taken.
*/
object *fn_gc(object *obj, object *env);

/*
  (room)
  Returns the number of free Lisp cells remaining.
*/
object *fn_room(object *args, object *env);

/*
  (save-image [symbol])
  Saves the current uLisp image to non-volatile memory or SD card so it can be loaded using
  load-image.
*/
object *fn_saveimage(object *args, object *env);

/*
  (load-image [filename])
  Loads a saved uLisp image from non-volatile memory or SD card.
*/
object *fn_loadimage(object *args, object *env);

/*
  (cls)
  Prints a clear-screen character.
*/
object *fn_cls(object *args, object *env);

// Arduino procedures

void Serialbegin(uint32_t baudrate);

bool Serialready(void);

void Serialwrite(char c);

void Serialflush(void);

char Serialread(void);

uint32_t Serialavailable(void);

uint64_t millis(void);

uint64_t micros(void);

uint32_t bitRead(uint32_t value, uint32_t index);

static inline uint32_t arduinoRandom(uint32_t max)
{
	return MAX(random(), max);
}

void pinMode(uint32_t pin, uint32_t mode);

uint32_t digitalRead(uint32_t pin);

void digitalWrite(uint32_t pin, uint32_t value);

uint32_t analogRead(uint32_t pin);

void analogWrite(uint32_t pin, uint32_t value);

void analogWriteResolution(uint32_t bits);

static inline void delay(uint32_t ms);

/*
  (pinmode pin mode)
  Sets the input/output mode of an Arduino pin number, and returns nil.
  The mode parameter can be an integer, a keyword, or t or nil.
*/
object *fn_pinmode(object *args, object *env);

/*
  (digitalread pin)
  Reads the state of the specified Arduino pin number and returns t (high) or nil (low).
*/
object *fn_digitalread(object *args, object *env);

/*
  (digitalwrite pin state)
  Sets the state of the specified Arduino pin number.
*/
object *fn_digitalwrite(object *args, object *env);

/*
  (analogread pin)
  Reads the specified Arduino analogue pin number and returns the value.
*/
object *fn_analogread(object *args, object *env);

/*
  (analogreference keyword)
  Specifies a keyword to set the analogue reference voltage used for analogue input.
*/
object *fn_analogreference(object *args, object *env);

/*
  (analogreadresolution bits)
  Specifies the resolution for the analogue inputs on platforms that support it.
  The default resolution on all platforms is 10 bits.
*/
object *fn_analogreadresolution(object *args, object *env);

/*
  (analogwrite pin value)
  Writes the value to the specified Arduino pin number.
*/
object *fn_analogwrite(object *args, object *env);

/*
  (analogwrite pin value)
  Sets the analogue write resolution.
*/
object *fn_analogwriteresolution(object *args, object *env);

/*
  (delay number)
  Delays for a specified number of milliseconds.
*/
object *fn_delay(object *args, object *env);

/*
  (millis)
  Returns the time in milliseconds that uLisp has been running.
*/
object *fn_millis(object *args, object *env);

/*
  (sleep secs)
  Puts the processor into a low-power sleep mode for secs.
  Only supported on some platforms. On other platforms it does delay(1000*secs).
*/
object *fn_sleep(object *args, object *env);

/*
  (note [pin] [note] [octave])
  Generates a square wave on pin.
  The argument note represents the note in the well-tempered scale, from 0 to 11,
  where 0 represents C, 1 represents C#, and so on.
  The argument octave can be from 3 to 6. If omitted it defaults to 0.
*/
object *fn_note(object *args, object *env);

/*
  (register address [value])
  Reads or writes the value of a peripheral register.
  If value is not specified the function returns the value of the register at address.
  If value is specified the value is written to the register at address and the function returns
  value.
*/
object *fn_register(object *args, object *env);

// Tree Editor

/*
  (edit 'function)
  Calls the Lisp tree editor to allow you to edit a function definition.
*/
object *fn_edit(object *args, object *env);

// Pretty printer

/*
  (pprint item [str])
  Prints its argument, using the pretty printer, to display it formatted in a structured way.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprint(object *args, object *env);

/*
  (pprintall [str])
  Pretty-prints the definition of every function and variable defined in the uLisp workspace.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprintall(object *args, object *env);

// Format

/*
  (format output controlstring [arguments]*)
  Outputs its arguments formatted according to the format directives in controlstring.
*/
object *fn_format(object *args, object *env);

// LispLibrary

/*
  (require 'symbol)
  Loads the definition of a function defined with defun, or a variable defined with defvar, from the
  Lisp Library. It returns t if it was loaded, or nil if the symbol is already defined or isn't
  defined in the Lisp Library.
*/
object *fn_require(object *args, object *env);

/*
  (list-library)
  Prints a list of the functions defined in the List Library.
*/
object *fn_listlibrary(object *args, object *env);

// Documentation

/*
  (? item)
  Prints the documentation string of a built-in or user-defined function.
*/
object *sp_help(object *args, object *env);

/*
  (documentation 'symbol [type])
  Returns the documentation string of a built-in or user-defined function. The type argument is
  ignored.
*/
object *fn_documentation(object *args, object *env);

/*
  (apropos item)
  Prints the user-defined and built-in functions whose names contain the specified string or symbol.
*/
object *fn_apropos(object *args, object *env);

/*
  (apropos-list item)
  Returns a list of user-defined and built-in functions whose names contain the specified string or
  symbol.
*/
object *fn_aproposlist(object *args, object *env);

// Error handling

/*
  (unwind-protect form1 [forms]*)
  Evaluates form1 and forms in order and returns the value of form1,
  but guarantees to evaluate forms even if an error occurs in form1.
*/
object *sp_unwindprotect(object *args, object *env);

/*
  (ignore-errors [forms]*)
  Evaluates forms ignoring errors.
*/
object *sp_ignoreerrors(object *args, object *env);

/*
  (error controlstring [arguments]*)
  Signals an error. The message is printed by format using the controlstring and arguments.
*/
object *sp_error(object *args, object *env);

// Wi-Fi

/*
  (with-client (str [address port]) form*)
  Evaluates the forms with str bound to a wifi-stream.
*/
object *sp_withclient(object *args, object *env);

/*
  (available stream)
  Returns the number of bytes available for reading from the wifi-stream, or zero if no bytes are
  available.
*/
object *fn_available(object *args, object *env);

/*
  (wifi-server)
  Starts a Wi-Fi server running. It returns nil.
*/
object *fn_wifiserver(object *args, object *env);

/*
  (wifi-softap ssid [password channel hidden])
  Set up a soft access point to establish a Wi-Fi network.
  Returns the IP address as a string or nil if unsuccessful.
*/
object *fn_wifisoftap(object *args, object *env);

/*
  (connected stream)
  Returns t or nil to indicate if the client on stream is connected.
*/
object *fn_connected(object *args, object *env);

/*
  (wifi-localip)
  Returns the IP address of the local network as a string.
*/
object *fn_wifilocalip(object *args, object *env);

/*
  (wifi-connect [ssid pass])
  Connects to the Wi-Fi network ssid using password pass. It returns the IP address as a string.
*/
object *fn_wificonnect(object *args, object *env);

// Graphics functions

/*
  (with-gfx (str) form*)
  Evaluates the forms with str bound to an gfx-stream so you can print text
  to the graphics display using the standard uLisp print commands.
*/
object *sp_withgfx(object *args, object *env);

/*
  (draw-pixel x y [colour])
  Draws a pixel at coordinates (x,y) in colour, or white if omitted.
*/
object *fn_drawpixel(object *args, object *env);

/*
  (draw-line x0 y0 x1 y1 [colour])
  Draws a line from (x0,y0) to (x1,y1) in colour, or white if omitted.
*/
object *fn_drawline(object *args, object *env);

/*
  (draw-rect x y w h [colour])
  Draws an outline rectangle with its top left corner at (x,y), with width w,
  and with height h. The outline is drawn in colour, or white if omitted.
*/
object *fn_drawrect(object *args, object *env);

/*
  (fill-rect x y w h [colour])
  Draws a filled rectangle with its top left corner at (x,y), with width w,
  and with height h. The outline is drawn in colour, or white if omitted.
*/
object *fn_fillrect(object *args, object *env);

/*
  (draw-circle x y r [colour])
  Draws an outline circle with its centre at (x, y) and with radius r.
  The circle is drawn in colour, or white if omitted.
*/
object *fn_drawcircle(object *args, object *env);

/*
  (fill-circle x y r [colour])
  Draws a filled circle with its centre at (x, y) and with radius r.
  The circle is drawn in colour, or white if omitted.
*/
object *fn_fillcircle(object *args, object *env);

/*
  (draw-round-rect x y w h radius [colour])
  Draws an outline rounded rectangle with its top left corner at (x,y), with width w,
  height h, and corner radius radius. The outline is drawn in colour, or white if omitted.
*/
object *fn_drawroundrect(object *args, object *env);

/*
  (fill-round-rect x y w h radius [colour])
  Draws a filled rounded rectangle with its top left corner at (x,y), with width w,
  height h, and corner radius radius. The outline is drawn in colour, or white if omitted.
*/
object *fn_fillroundrect(object *args, object *env);

/*
  (draw-triangle x0 y0 x1 y1 x2 y2 [colour])
  Draws an outline triangle between (x1,y1), (x2,y2), and (x3,y3).
  The outline is drawn in colour, or white if omitted.
*/
object *fn_drawtriangle(object *args, object *env);

/*
  (fill-triangle x0 y0 x1 y1 x2 y2 [colour])
  Draws a filled triangle between (x1,y1), (x2,y2), and (x3,y3).
  The outline is drawn in colour, or white if omitted.
*/
object *fn_filltriangle(object *args, object *env);

/*
  (draw-char x y char [colour background size])
  Draws the character char with its top left corner at (x,y).
  The character is drawn in a 5 x 7 pixel font in colour against background,
  which default to white and black respectively.
  The character can optionally be scaled by size.
*/
object *fn_drawchar(object *args, object *env);

/*
  (set-cursor x y)
  Sets the start point for text plotting to (x, y).
*/
object *fn_setcursor(object *args, object *env);

/*
  (set-text-color colour [background])
  Sets the text colour for text plotted using (with-gfx ...).
*/
object *fn_settextcolor(object *args, object *env);

/*
  (set-text-size scale)
  Scales text by the specified size, default 1.
*/
object *fn_settextsize(object *args, object *env);

/*
  (set-text-wrap boolean)
  Specified whether text wraps at the right-hand edge of the display; the default is t.
*/
object *fn_settextwrap(object *args, object *env);

/*
  (fill-screen [colour])
  Fills or clears the screen with colour, default black.
*/
object *fn_fillscreen(object *args, object *env);

/*
  (set-rotation option)
  Sets the display orientation for subsequent graphics commands; values are 0, 1, 2, or 3.
*/
object *fn_setrotation(object *args, object *env);

/*
  (invert-display boolean)
  Mirror-images the display.
*/
object *fn_invertdisplay(object *args, object *env);

const tbl_entry_t *table(int n);

unsigned int tablesize(int n);

// Table lookup functions

/*
  lookupbuiltin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
builtin_t lookupbuiltin(char *c);

/*
  lookupfn - looks up the entry for name in lookup_table[], and returns the function entry point
*/
intptr_t lookupfn(builtin_t name);

/*
  getminmax - gets the minmax byte from lookup_table[] whose octets specify the type of function
  and minimum and maximum number of arguments for name
*/
uint8_t getminmax(builtin_t name);

/*
  checkminmax - checks that the number of arguments nargs for name is within the range specified by
  minmax
*/
void checkminmax(builtin_t name, int nargs);

/*
  lookupdoc - looks up the documentation string for the built-in function name
*/
char *lookupdoc(builtin_t name);

/*
  findsubstring - tests whether a specified substring occurs in the name of a built-in function
*/
boolean findsubstring(char *part, builtin_t name);

/*
  testescape - tests whether the '~' escape character has been typed
*/
void testescape();

/*
  keywordp - check that obj is a keyword
*/
bool keywordp(object *obj);

// Main evaluator

#define ENDSTACK end

extern uint32_t ENDSTACK; // Bottom of stack

/*
  eval - the main Lisp evaluator
*/
object *eval(object *form, object *env);

// Print functions

/*
  pserial - prints a character to the serial port
*/
void pserial(char c);

/*
  pcharacter - prints a character to a stream, escaping special characters if PRINTREADABLY is false
  If <= 32 prints character name; eg #\Space
  If < 127 prints ASCII; eg #\A
  Otherwise prints decimal; eg #\234
*/
void pcharacter(uint8_t c, pfun_t pfun);

/*
  pstring - prints a C string to the specified stream
*/
void pstring(char *s, pfun_t pfun);

/*
  plispstring - prints a Lisp string object to the specified stream
*/
void plispstring(object *form, pfun_t pfun);

/*
  plispstr - prints a Lisp string name to the specified stream
*/
void plispstr(symbol_t name, pfun_t pfun);

/*
  printstring - prints a Lisp string object to the specified stream
  taking account of the PRINTREADABLY flag
*/
void printstring(object *form, pfun_t pfun);

/*
  pbuiltin - prints a built-in symbol to the specified stream
*/
void pbuiltin(builtin_t name, pfun_t pfun);

/*
  pradix40 - prints a radix 40 symbol to the specified stream
*/
void pradix40(symbol_t name, pfun_t pfun);

/*
  printsymbol - prints any symbol from a symbol object to the specified stream
*/
void printsymbol(object *form, pfun_t pfun);

/*
  psymbol - prints any symbol from a symbol name to the specified stream
*/
void psymbol(symbol_t name, pfun_t pfun);

/*
  pfstring - prints a string from flash memory to the specified stream
*/
void pfstring(const char *s, pfun_t pfun);

/*
  pint - prints an integer in decimal to the specified stream
*/
void pint(int i, pfun_t pfun);

/*
  pintbase - prints an integer in base 'base' to the specified stream
*/
void pintbase(uint32_t i, uint8_t base, pfun_t pfun);

/*
  pinthex4 - prints a four-digit hexadecimal number with leading zeros to the specified stream
*/
void printhex4(int i, pfun_t pfun);

/*
  pmantissa - prints the mantissa of a floating-point number to the specified stream
*/
void pmantissa(float f, pfun_t pfun);

/*
  pfloat - prints a floating-point number to the specified stream
*/
void pfloat(float f, pfun_t pfun);

/*
  pln - prints a newline to the specified stream
*/
inline void pln(pfun_t pfun);

/*
  pfl - prints a newline to the specified stream if a newline has not just been printed
*/
void pfl(pfun_t pfun);

/*
  plist - prints a list to the specified stream
*/
void plist(object *form, pfun_t pfun);

/*
  pstream - prints a stream name to the specified stream
*/
void pstream(object *form, pfun_t pfun);

/*
  printobject - prints any Lisp object to the specified stream
*/
void printobject(object *form, pfun_t pfun);

/*
  prin1object - prints any Lisp object to the specified stream escaping special characters
*/
void prin1object(object *form, pfun_t pfun);

// Read functions

/*
  glibrary - reads a character from the Lisp Library
*/
int glibrary();

/*
  loadfromlibrary - reads and evaluates a form from the Lisp Library
*/
void loadfromlibrary(object *env);

// Parenthesis highlighting
void esc(int p, char c);

void hilight(char c);

/*
  Highlight - handles parenthesis highlighting with the line editor
*/
void Highlight(int p, int wp, uint8_t invert);

/*
  processkey - handles keys in the line editor
*/
void processkey(char c);

/*
  gserial - gets a character from the serial port
*/
int gserial();

/*
  nextitem - reads the next token from the specified stream
*/
object *nextitem(gfun_t gfun);

/*
  readrest - reads the remaining tokens from the specified stream
*/
object *readrest(gfun_t gfun);

/*
  read - recursively reads a Lisp object from the stream gfun and returns it
*/
object *read(gfun_t gfun);

// Setup

/*
  initenv - initialises the uLisp environment
*/
void initenv();

/*
  initgfx - initialises the graphics
*/
void initgfx();

// Entry point from the Arduino IDE
void setup(void);

// Read/Evaluate/Print loop

/*
  repl - the Lisp Read/Evaluate/Print loop
*/
void repl(object *env);

/*
  loop - the Arduino IDE main execution loop
*/
void loop(void);

void ulispreset();

int main(void);
