/*** includes ***/
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h> // gives us iscntrl()
#include <errno.h> // gives us access to errno variable
#include <fcntl.h> //open, O_CREAT, ORDWR
#include <stdio.h> // gives us printf(), perror(), sscanf()
#include <stdarg.h> //
#include <stdlib.h> // gives us atexit(), exit(), realloc(), free(), malloc()
#include <sys/ioctl.h> // gives us ioctl(), TIOCGWINSZ
#include <sys/types.h> // ssize_t
#include <termios.h> // gives us terminal attrs (ECHO, ISIG, ICANON) and VMIN/VTIME
#include <time.h> // time()
#include <unistd.h> // gives us read, stdout_fileno, write
#include <string.h> // memcpy()

#include "kilo.h"

/*** prototypes ***/
void editorSetStatusMessage(const char *fmt, ...);
void editorDelRow(int at);

/*** defines ***/
#define KILO_VERSION "0.0.1"
#define CTRL_KEY(k) ((k) & 0x1f) // essentially applies 0001111 bitmask - ctrl-A = 'A'-64 (6th bit) - (5th bit sets upper/lower)
#define ABUF_INIT {NULL, 0} // const that represents an empty buffer (constructor)
#define TAB_SIZE 4
#define KILO_QUIT_TIMES 3

enum editorKey {
	BACKSPACE = 127,
	ARROW_LEFT = 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
};

/*** data ***/
typedef struct erow {
	int size;
	char *chars;
	int rsize;
	char *render;
} erow;

struct editorConfig {
	int cx_cached, cy_cached;
	int cx, cy; // index into chars
	int rx; //index into render
	int screenrows;
	int screencols;
	int rowoff;
	int coloff;
	int numrows;
	erow *row;
	struct termios orig_term_attrs;	
	int dirty;
	// status bar
	char *filename;
	char statusmsg[80];
	time_t statusmsg_time;
	int statusmsg_visible;
};

struct editorConfig E;

/*** terminal ***/
void die(const char *s) {
	write(STDOUT_FILENO, "\x1b[2J", 4);
	write(STDOUT_FILENO, "\x1b[H", 3);
	perror(s); //print user input & error (tracked in internal global `errno` var)
	exit(1);
}

void disableRawMode(void) {
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_term_attrs) == -1)
		die("tcsetattr");
}

void enableRawMode(void) {
	if(tcgetattr(STDIN_FILENO, &E.orig_term_attrs) == -1) // store terminal attr obj in a termios struct
		die("tcgetattr"); // wrap in error handling
	atexit(disableRawMode);

	struct termios raw = E.orig_term_attrs;

	// terminal attrs
	raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN); // 2. "local flags". ECHO is a bitflag of 0s except the fourth bit. the ~ and & ops force the fourth bit of flags to become 0 w/o changing the other fields, turning ECHO off. ICANON is canonical mode. ISIG is the ctrl-c/ctrl-z SIGINT suspensions. IEXTEN V, O (wait to output next letter literally)
	raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP); // input flags: turn off ctrl-s/ctrl-q XOFF and XON transmission controls. and ctrl-M (newline). last 3 mostly historical
	raw.c_oflag &= ~(OPOST); // output processing—stop auto-adding carriage returns
	raw.c_cflag |= (CS8); //sets char size to 8 bits per byte

	//read() timing
	raw.c_cc[VMIN] = 0; // min chars for read to exit
	raw.c_cc[VTIME] = 1; //1/10 of a second
	
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editorReadKey(void) {
	int nread;
	char c;
	while((nread = read(STDIN_FILENO, &c, 1))!=1) {
		if(nread== -1 && errno != EAGAIN) die("read");
	}

	if(c=='\x1b') { // escape sequence
		char seq[3];
		if(read(STDIN_FILENO,&seq[0],1)!=1) return '\x1b'; // read "[", time out otherwise
		if(read(STDIN_FILENO,&seq[1],1)!=1) return '\x1b'; // read arg

		if(seq[0]=='[') {
			if(seq[1]>='0' && seq[1]<='9') { //if command is numeric instead of alphabetic
				if(read(STDIN_FILENO,&seq[2],1)!=1) return '\x1b';
				if(seq[2]=='~') {
					switch(seq[1]) {
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
				} else {
				switch(seq[1]) {
					case 'A': return ARROW_UP;
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
		} else { // no bracket, ie \OF 
			if(seq[0]=='O') {
				switch(seq[1]) {
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
		}
		return '\x1b'; // if unknown
	} else { 
		return c;
	}
}

int getWindowSize(int *rows, int*cols) {
	struct winsize ws;

	if(ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
		if(write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
		return getCursorPosition(rows,cols);
	} else {
		*cols = ws.ws_col;
		*rows = ws.ws_row;
		return 0;
	}
}

/*** row operations ***/
int editorRowCxToRx(erow *row, int cx) {
	int rx = 0;
	for(int j=0;j<cx;++j) {
		if(row->chars[j]=='\t') {
			rx += (TAB_SIZE - 1) - (rx % TAB_SIZE);
		}
		++rx;
	}
	return rx;
}

void editorUpdateRowRender(erow *row) {
	int tabCount=0;
	for(int i=0;i<row->size;++i) {
		if(row->chars[i]=='\t') ++tabCount;
	}
	free(row->render);
	row->render = malloc(row->size + tabCount*(TAB_SIZE-1) + 1);
	
	int i;
	int idx=0;
	for(i=0;i<row->size;++i) {
		if(row->chars[i]=='\t') {
			row->render[idx++] = ' ';
			while(idx%TAB_SIZE!=0) row->render[idx++] = ' ';
		} else{
			row->render[idx++] = row->chars[i];
		}
	}
	row->render[idx] = '\0';
	row->rsize = idx;
}

void editorInsertRow(int at, char *s, size_t len) {
	if(at<0 || at>E.numrows) return;

	E.row = realloc(E.row, sizeof(erow) * (E.numrows+1));
	memmove(&E.row[at+1],&E.row[at], sizeof(erow)*(E.numrows-at));

	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1);
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';

	E.row[at].render = NULL;
	E.row[at].rsize = 0;
	editorUpdateRowRender(&E.row[at]);

	E.numrows++;
}

void editorInsertNewline(void) {
	if(E.cx==0) {
		editorInsertRow(E.cy,"",0);
	} else {
		erow *row = &E.row[E.cy];
		editorInsertRow(E.cy+1,&row->chars[E.cx],row->size-E.cx);
		row = &E.row[E.cy]; // reset ptr bc realloc might move memory to diff block & invalidate ptr
		row->size=E.cx;
		row->chars[E.cx] = '\0';
		editorUpdateRowRender(row);
	}
	E.cy++;
	E.cx=0;
	E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
	if(at<0) die("editorRowInsertChar");
	if(at>row->size) at = row->size;

	row->chars = realloc(row->chars,row->size+2);
	memmove(&row->chars[at+1],&row->chars[at],row->size-at+1); //like memcpy but for overlapping memory in dest/src
	row->size++;
	row->chars[at]=c;
	editorUpdateRowRender(row);
}

void editorRowDelChar(erow *row, int at) {
	if(at < 0 || at >= row->size) return;
	memmove(&row->chars[at],&row->chars[at+1], row->size-at);
	row->size--;
	editorUpdateRowRender(row);
	E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
	row->chars = realloc(row->chars, row->size + len + 1);
	memcpy(&row->chars[row->size], s, len);
	row->size+=len;
	row->chars[row->size] = '\0';
	editorUpdateRowRender(row);
	E.dirty++;
}

/*** editor operations ***/
void editorInsertChar(int c) {
	if(E.cy == E.numrows) {
		editorInsertRow(E.numrows,"",0);
	}
	editorRowInsertChar(&E.row[E.cy], E.cx, c);
	E.cx++;
	E.dirty++;
}

void editorDelChar(void) {
	if(E.cy == E.numrows) return; //past end of file
	if(E.cy==0 && E.cx==0) return; //can't delete anything

	erow *row = &E.row[E.cy];
	if(E.cx > 0) {
		E.cx--;
		editorRowDelChar(row, E.cx);
	} else {
		E.cx = E.row[E.cy-1].size;
		editorRowAppendString(&E.row[E.cy-1], row->chars, row->size);
		editorDelRow(E.cy);
		E.cy--;
	}
}

void editorFreeRow(erow *row) {
	free(row->render);
	free(row->chars);
}

void editorDelRow(int at) {
	if(at < 0 || at >= E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at],&E.row[at+1],sizeof(erow) * (E.numrows - at -1)); // shuffle all rows behind it up
	E.numrows--;
	E.dirty++;
}

/*** file IO ***/
void editorOpen(char *filename) {
	free(E.filename);
	E.filename = strdup(filename);

	FILE *fp = fopen(filename, "r");
	if(!fp) die("fopen");

	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;

	while( (linelen = getline(&line, &linecap, fp)) !=-1) {
		while(linelen>0 && (line[linelen-1]=='\n' || line[linelen-1]=='\r')) linelen--;
		editorInsertRow(E.numrows,line, linelen);
	}
	free(line);
	fclose(fp);
	
}

char* editorRowsToString(int* buflen) {
	int totlen = 0;
	for(int j=0;j<E.numrows;j++) {
		totlen += E.row[j].size + 1;
	}
	*buflen = totlen;
	
	char* buf = malloc(totlen);
	char* cur = buf;
	for(int j=0;j<E.numrows;j++) {
		memcpy(cur, E.row[j].chars, E.row[j].size);
		cur+=E.row[j].size;
		*cur = '\n';
		cur++;
	}

	return buf;
}

void editorSave(void) {
	if(E.filename==NULL) return;

	int len;
	char *buf = editorRowsToString(&len);
	int fd = open(E.filename, O_RDWR | O_CREAT, 0644); // create file if DNE, read/write mode, 0644 = owner has permission to read/write, others have read perm
	if(fd != -1) {
		if(ftruncate(fd, len) != -1) { // delete file contents until len, or pads to len
			if(write(fd, buf, len) != -1) { // writes len characters from buf into fd
				close(fd);
				free(buf);
				editorSetStatusMessage("%d bytes written to disk", len);
				E.dirty = 0;
				return;
			}
		}
		close(fd);
	}
	free(buf);
	editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno)); // strerror indexes errno into the associated message
}
/*** append buffer ***/
struct abuf {
	char *b;
	int len;
};

void abAppend(struct abuf *ab, const char *s, int len) {
	char *new = realloc(ab->b, ab->len + len);

	if(new==NULL)return; // if out of memory or bad poitner input

	memcpy(&new[ab->len], s, len);
	ab->b = new;
	ab->len += len;
}

void abFree(struct abuf *ab) {
	free(ab->b);
}

/*** output ***/
void editorScroll(void) {
	E.rx = E.cx; 
	if(E.cy < E.numrows) {
		E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
	}

	if (E.cy < E.rowoff) {
		E.rowoff = E.cy;
	}
	if(E.cy >= E.rowoff + E.screenrows) {
		E.rowoff = E.cy - E.screenrows + 1;
	}

	if(E.rx < E.coloff) {
		E.coloff = E.rx;
	}
	if(E.rx >= E.coloff + E.screencols) {
		E.coloff = E.rx - E.screencols + 1;
	}
}

void editorDrawMessageBar(struct abuf *ab) {
	struct abuf ab2 = ABUF_INIT;

	abAppend(&ab2, "\r\n", 2);
	abAppend(&ab2, "\x1b[K", 3);
	int msglen = strlen(E.statusmsg);
	if(msglen > E.screencols) msglen = E.screencols;
	if(msglen && time(NULL) < E.statusmsg_time + 2) {
		abAppend(ab, ab2.b, ab2.len);
		abAppend(ab, E.statusmsg, msglen);
	} else if(E.statusmsg_visible) {
		E.screenrows++;
		E.statusmsg_visible = 0;
	}

	abFree(&ab2);
}

void editorSetStatusMessage(const char *fmt, ...) {
	va_list ap;
	va_start(ap,fmt);
	vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
	va_end(ap);
	E.statusmsg_time = time(NULL);

	if(!E.statusmsg_visible) E.screenrows--;
	E.statusmsg_visible = 1;
}

void editorDrawStatusBar(struct abuf *ab) {
	abAppend(ab, "\x1b[7m",4);

	char status[80];
	char rstatus[50];
	int len=snprintf(status, sizeof(status), "%.20s%s - %d lines", 
				E.filename ? E.filename : "[Untitled]", 
				E.dirty ? " (modified)" : "", 
				E.numrows);
	int rlen=snprintf(rstatus, sizeof(rstatus), "%d/%d",E.cy,E.numrows);
	if(len>E.screencols) len = E.screencols;
	abAppend(ab, status, len);

	while(len<E.screencols) {
		if(len==E.screencols-rlen) { // at the right edge
			abAppend(ab, rstatus, rlen);
			break;
		} else {
			abAppend(ab," ",1);
			len++;	
		}
	}
	abAppend(ab,"\x1b[m",3);
}
void editorDrawRows(struct abuf *ab) {
	int y;

	for(y=0;y<E.screenrows;y++) {
		int filerow = y + E.rowoff;
		if(filerow >= E.numrows){
			if (E.numrows ==0 && y == 4 * E.screenrows / 5) {
				char welcome[80];
				int welcomelen = snprintf(welcome, sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);
				if(welcomelen > E.screencols) welcomelen = E.screencols; // truncate

				// center horizontally
				int padding = (E.screencols - welcomelen) / 2;
				if (padding) {
					abAppend(ab, "~", 1);
					padding--;
				}
				while(padding--) abAppend(ab, " ", 1);

				abAppend(ab, welcome, welcomelen);
			} else {
				abAppend(ab, "~", 1);
			}
		} else {
			int len = E.row[filerow].rsize - E.coloff;
			if(len>E.screencols) len = E.screencols;
			if(len<0) len = 0;
			abAppend(ab, &E.row[filerow].render[E.coloff], len);
		}
		
		abAppend(ab, "\x1b[K", 3);
		abAppend(ab, "\r\n", 2);
	}
}

void editorRefreshScreen(void) {
	editorScroll();

	struct abuf ab = ABUF_INIT;

	abAppend(&ab, "\x1b[?25l", 6); // hide the cursor
	// remove bc clearing by line instead: abAppend(&ab, "\x1b[2J", 4); // write 4 bytes. escape sequence = escape char (\x1b) + [. then use J command w arg 2, clear entire screen. 1clears screen to cursor, 0 clears from cursor to end of screen (default). VT100 escape sequences
	abAppend(&ab, "\x1b[H", 3); //positions cursor at first row/col
	
	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);

	// reset cursor to cx,cy
	char buf[32];
	snprintf(buf,sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff)+1,(E.rx - E.coloff)+1);
	abAppend(&ab, buf, strlen(buf));
	abAppend(&ab, "\x1b[?25h", 6); // unhide the cursor

	write(STDOUT_FILENO, ab.b, ab.len);
	abFree(&ab);
}

int getCursorPosition(int *rows, int *cols) {
	char buf[32];
	unsigned int i = 0;

	if(write(STDOUT_FILENO, "\x1b[6n",4) != 4) return -1;  // use command n, arg 6. 

	while(i<sizeof(buf)-1) {
		if(read(STDIN_FILENO, &buf[i], 1)==-1) break;
		if(buf[i]=='R') break;
		++i;
	}
	buf[i] = '\0';

	if(buf[0]!='\x1b' || buf[1]!='[') return -1;
	if(sscanf(&buf[2],"%d;%d",rows,cols) != 2) return -1;

	return 0;
}

/*** input ***/
void editorMoveCursor(int key) {
	erow *row = E.cy >= E.numrows ? NULL : &E.row[E.cy];

	switch(key) {
		case ARROW_UP:
			if(E.cy!=0) E.cy--;
			break;			
		case ARROW_LEFT:
			if(E.cx!=0) E.cx--;
			else if(E.cy!=0) {
				E.cy--;
				E.cx = E.row[E.cy].size;
			}
			E.cx_cached = -1;
			break;
		case ARROW_DOWN:
			if(E.cy < E.numrows) {
				E.cy++;
			}
			break;
		case ARROW_RIGHT:
			if(row) {
				if(E.cx < row->size) E.cx++;
				else {
					E.cy++;
					E.cx = 0;
				}
			}
			E.cx_cached = -1;
			break;
	}

	row = E.cy >= E.numrows ? NULL : &E.row[E.cy];
	int rowlen = row ? row->size : 0;
	if(E.cx > rowlen) {
		if(E.cx_cached < E.cx) E.cx_cached = E.cx;
		E.cx = rowlen;
	} else if(E.cx_cached!=-1) {
		int newlen = rowlen < E.cx_cached ? rowlen : E.cx_cached;
		E.cx = newlen;
	}
}

void editorProcessKeypress(void) {
	static int quit_times = KILO_QUIT_TIMES;
	int c = editorReadKey();
	switch(c) {
		case CTRL_KEY('q'):
			if(E.dirty && quit_times > 0) {
				editorSetStatusMessage("Warning: Editor has unsaved changes. Press Ctrl-Q %d more times to quit.", quit_times);
				quit_times--;
				return;
			}

			write(STDOUT_FILENO, "\x1b[2J",4);
			write(STDOUT_FILENO, "\x1b[H", 3); 
			exit(0);
			break;

		case CTRL_KEY('s'):
			editorSave();
			break;

		case '\r': //enter key
			editorInsertNewline();
			break;

		case HOME_KEY: 
			E.cx = 0;
			E.cx_cached = -1;
			break;
		case END_KEY:
			if(E.cy < E.numrows) {
				E.cx = E.row[E.cy].size;
			}
			E.cx_cached=-1;
			break;
		
		case BACKSPACE:
		case CTRL_KEY('h'): // old backspace char
		case DEL_KEY:
			if(c == DEL_KEY) editorMoveCursor(ARROW_RIGHT); // have my doubts abt this behavior
			editorDelChar();
			break;

		case PAGE_DOWN:
		case PAGE_UP:
			{
				// if(c==PAGE_UP) {
				// 	E.cy = E.rowoff;
				// } else if (c==PAGE_DOWN) {
				// 	E.cy = E.rowoff + E.screenrows - 1;
				// 	if(E.cy > E.numrows) E.cy = E.numrows;
				// }

				int shifts = E.screenrows;
				while(shifts--) {
					editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
				}
			}


		case ARROW_UP:
		case ARROW_LEFT:
		case ARROW_DOWN:
		case ARROW_RIGHT:
			editorMoveCursor(c);
			break;

		case CTRL_KEY('l'): // screen refresh, alr handled
		case '\x1b': //escape char
			/* ignore */
			break;
		default:
			editorInsertChar(c);
			break;
	}

	quit_times = KILO_QUIT_TIMES;
}

/*** init ***/
void initEditor(void) { // init everything in E
	if(getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
	E.screenrows--; // for status bar

	E.cx = 0;
	E.cy = 0;
	E.rx = 0;
	E.rowoff = 0;
	E.coloff = 0;
	E.numrows = 0;
	E.row = NULL;
	E.dirty = 0;
	E.filename = NULL;
	E.statusmsg[0] = '\0';
	E.statusmsg_time = 0;
}

int main(int argc, char * argv[]) {
	enableRawMode();
	initEditor();

	if(argc >= 2) editorOpen(argv[1]);
	editorSetStatusMessage("HELP: Ctrl-Q = quit");

	while(1) { // 1. reads user input into the character c one letter at a time until read() returns 0, which is triggered by ctrl-D.
		editorRefreshScreen();
		editorProcessKeypress();
	}
	return 0;
}
