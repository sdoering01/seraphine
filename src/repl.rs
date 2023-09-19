use std::{
    cell::OnceCell,
    collections::VecDeque,
    fmt::Display,
    fs::{File, OpenOptions},
    io::Write,
};

use termion::{
    self, color,
    cursor::DetectCursorPos,
    event::{Event, Key},
    input::TermRead,
    raw::{IntoRawMode, RawTerminal},
};

use crate::{
    error::ParseError,
    eval::Context,
    tokenizer::{Token, TokenKind},
    CalcError,
};

static mut DEBUG_FILE: OnceCell<File> = OnceCell::new();

macro_rules! debug {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            let mut debug_file = unsafe { DEBUG_FILE.get_or_init(|| {
                OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open("/tmp/seraphine-debug-output.txt")
                    .expect("couldn't open debug file")
            })};
            write!(debug_file, $($arg)*)?;
        }
    };
}

const PREFIX: &str = "> ";
const INPUT_INCOMPLETE_PREFIX: &str = "| ";

const WELCOME_MESSAGE: &str = "\
Welcome to Seraphine!
Type \".help\" for help.
";

const HELP_MESSAGE: &str = "\
Left/Right: move cursor
Up/Down: move through history
Ctrl+W: delete word before cursor
Ctrl+U: delete before cursor
Ctrl+L: clear screen
Ctrl+C: clear current input
Ctrl+D: exit (if input is empty)
";

#[derive(Debug)]
struct ReplWriter {
    stdout: std::io::Stdout,
    fg_color_string: Option<String>,
}

impl Write for ReplWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Some(color_str) = &self.fg_color_string {
            self.stdout.write_all(color_str.as_bytes())?;
        }

        let mut line_start_idx = 0;
        let mut idx = 0;
        for b in buf {
            if *b == b'\n' {
                self.stdout.write_all(&buf[line_start_idx..idx])?;
                write_newline(&mut self.stdout)?;
                line_start_idx = idx + 1;
            }
            idx += 1;
        }

        // Write the rest of the buffer
        self.stdout.write_all(&buf[line_start_idx..idx])?;

        if self.fg_color_string.is_some() {
            self.stdout.write_all(color::Reset.fg_str().as_bytes())?;
            self.flush()?;
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stdout.flush()
    }
}

impl ReplWriter {
    fn with_color(fg_color_string: impl Into<String>) -> ReplWriter {
        ReplWriter {
            stdout: std::io::stdout(),
            fg_color_string: Some(fg_color_string.into()),
        }
    }
}

struct History {
    max_size: usize,
    entries: VecDeque<String>,
}

impl History {
    fn new(max_size: usize) -> History {
        History {
            max_size,
            entries: VecDeque::new(),
        }
    }

    fn push(&mut self, entry: String) {
        if self.entries.len() == self.max_size {
            self.entries.pop_back();
        }

        self.entries.push_front(entry);
    }

    fn get(&self, idx: usize) -> Option<&str> {
        self.entries.get(idx).map(|e| e.as_str())
    }

    fn entries_len(&self) -> usize {
        self.entries.len()
    }
}

// Prevent accidental direct writes to stdout
struct WrappedStdout(RawTerminal<std::io::Stdout>);

struct Repl {
    ctx: Context,
    stdout: WrappedStdout,
    input: String,
    line: String,
    pos_in_line: usize,
    ctrl_c_pressed: bool,
    history: History,
    history_index: usize,
    prev_cursor_y_in_input: usize,
    prev_prompt_lines: u16,
    should_exit: bool,
    wrote_since_last_prompt: bool,
}

impl Repl {
    fn new() -> std::io::Result<Repl> {
        let ctx = Context::builder()
            .debug_writer(Some(ReplWriter::with_color(color::LightBlack.fg_str())))
            .build();

        let stdout = WrappedStdout(std::io::stdout().into_raw_mode()?);

        Ok(Repl {
            ctx,
            stdout,
            input: String::new(),
            line: String::new(),
            pos_in_line: 0,
            ctrl_c_pressed: false,
            history: History::new(1000),
            history_index: 0,
            prev_cursor_y_in_input: 0,
            prev_prompt_lines: 1,
            should_exit: false,
            wrote_since_last_prompt: false,
        })
    }

    fn handle_event(&mut self, ev: Event) -> std::io::Result<()> {
        if let Event::Key(key) = ev {
            if !matches!(key, Key::Ctrl('c')) {
                self.ctrl_c_pressed = false;
            }

            match key {
                Key::Ctrl('d') => {
                    if self.input.is_empty() && self.line.is_empty() {
                        self.should_exit = true;
                    }
                }
                Key::Ctrl('c') => {
                    if self.input.is_empty() && self.line.is_empty() {
                        if self.ctrl_c_pressed {
                            self.should_exit = true;
                        } else {
                            self.ctrl_c_pressed = true;
                            self.write_displayable("\npress ctrl+c again to exit")?;
                        }
                    } else {
                        self.input.clear();
                        self.line.clear();
                        self.pos_in_line = 0;
                        self.write_newline()?;
                    }
                }
                Key::Ctrl('l') => self.clear()?,
                Key::Char('\n') => {
                    self.write_newline()?;

                    let is_line_empty = self.line.trim().is_empty();

                    if !is_line_empty {
                        self.history.push(self.line.clone());
                        self.history_index = 0;
                        self.input.push_str(&self.line);
                    }

                    self.line.clear();
                    self.pos_in_line = 0;

                    if self.input.is_empty() && is_line_empty {
                        return Ok(());
                    }

                    if self.input == ".help" {
                        self.write_displayable(HELP_MESSAGE)?;
                        self.input.clear();
                        return Ok(());
                    }

                    // TODO: Implement this "properly", so that one can use repl functionality
                    // (move the cursor, delete word, ...) when reading from stdin
                    self.stdout.0.suspend_raw_mode()?;
                    let eval_result = self.ctx.eval_str(&self.input);
                    self.stdout.0.activate_raw_mode()?;

                    match eval_result {
                        Ok(result) => {
                            let (cursor_x, _) = self.stdout.0.cursor_pos()?;
                            if cursor_x > 1 {
                                self.write_newline()?;
                            }

                            self.write_displayable(result)?;
                            self.input.clear();
                        }
                        Err(CalcError::ParseError(
                            ParseError::NoTokensLeft
                            | ParseError::UnexpectedToken {
                                token:
                                    Token {
                                        kind: TokenKind::Eof,
                                        ..
                                    },
                                ..
                            },
                        )) => {
                            self.input.push('\n');
                        }
                        Err(err) => {
                            let formatted_err = err.format(&self.input, "<repl>");
                            self.write_error(formatted_err)?;
                            self.input.clear();
                        }
                    };
                }
                Key::Char(c) => {
                    // Currently only ASCII characters are supported
                    if c.len_utf8() > 1 {
                        return Ok(());
                    }

                    if c == '\t' {
                        let ident = "    ";
                        self.line.insert_str(self.pos_in_line, ident);
                        self.pos_in_line += ident.len();
                    } else {
                        self.line.insert(self.pos_in_line, c);
                        self.pos_in_line += 1;
                    }
                }
                Key::Left => {
                    self.pos_in_line = self.pos_in_line.saturating_sub(1);
                }
                Key::Right => {
                    if self.pos_in_line < self.line.len() {
                        self.pos_in_line += 1;
                    }
                }
                Key::Up => {
                    if self.history_index < self.history.entries_len() {
                        self.history_index += 1;
                        self.line = self
                            .history
                            .get(self.history_index - 1)
                            .unwrap_or_default()
                            .to_string();
                        self.pos_in_line = self.line.len();
                    }
                }
                Key::Down => {
                    if self.history_index > 0 {
                        self.history_index -= 1;

                        if self.history_index > 0 {
                            self.line = self
                                .history
                                .get(self.history_index - 1)
                                .unwrap_or_default()
                                .to_string();
                            self.pos_in_line = self.line.len();
                        } else {
                            self.line.clear();
                            self.pos_in_line = 0;
                        }
                    }
                }
                Key::Ctrl('w') => {
                    if self.pos_in_line > 0 {
                        let mut idx = self.pos_in_line - 1;

                        // Drain whitespaces
                        while idx > 0 && self.line.as_bytes()[idx] == b' ' {
                            idx -= 1;
                        }

                        // Drain word
                        if idx > 0 {
                            // We have to make sure that the index points to the first character of
                            // the word, not the character before it
                            idx -= 1;
                            while idx > 0 && self.line.as_bytes()[idx - 1] != b' ' {
                                idx -= 1;
                            }
                        }

                        self.line.drain(idx..self.pos_in_line);
                        self.pos_in_line = idx;
                    }
                }
                Key::Ctrl('u') => {
                    self.line.drain(..self.pos_in_line);
                    self.pos_in_line = 0;
                }
                Key::Backspace => {
                    if self.pos_in_line > 0 {
                        self.line.remove(self.pos_in_line - 1);
                        self.pos_in_line -= 1;
                    }
                }
                Key::Delete => {
                    if self.pos_in_line < self.line.len() {
                        self.line.remove(self.pos_in_line);
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn write_prompt(&mut self) -> std::io::Result<()> {
        let current_prefix = if !self.input.is_empty() {
            INPUT_INCOMPLETE_PREFIX
        } else {
            PREFIX
        };

        let (term_width, _) = termion::terminal_size()?;

        let mut prompt_length = current_prefix.len() + self.line.len();
        if self.pos_in_line == self.line.len() {
            // Account for case where the cursor requires to be placed after the last character
            prompt_length += 1;
        }

        let cursor_y_in_input = (current_prefix.len() + self.pos_in_line) / term_width as usize;
        // Round up
        let prompt_lines =
            ((prompt_length + term_width.saturating_sub(1) as usize) / term_width as usize) as u16;

        if prompt_lines > self.prev_prompt_lines {
            // Make space for another line, but leave cursor in the same y position
            let line_diff = self.lines_to_last_prompt_line();
            if line_diff > 0 {
                write!(self.stdout.0, "{}", termion::cursor::Down(line_diff))?;
            }
            write!(self.stdout.0, "\n{}", termion::cursor::Up(1 + line_diff))?;
        }

        let (_, current_cursor_y) = self.stdout.0.cursor_pos()?;
        let prompt_start_y = if self.wrote_since_last_prompt {
            current_cursor_y
        } else {
            current_cursor_y.saturating_sub(self.prev_cursor_y_in_input as u16)
        };
        let cursor_x = ((current_prefix.len() + self.pos_in_line) % term_width as usize) as u16 + 1;
        let cursor_y = (prompt_start_y as usize + cursor_y_in_input) as u16;

        self.prev_prompt_lines = prompt_lines;
        self.prev_cursor_y_in_input = cursor_y_in_input;

        write!(
            self.stdout.0,
            "{}{}{}{}{}",
            termion::cursor::Goto(1, prompt_start_y),
            termion::clear::AfterCursor,
            current_prefix,
            self.line,
            termion::cursor::Goto(cursor_x, cursor_y)
        )?;
        self.flush()?;

        self.wrote_since_last_prompt = false;

        Ok(())
    }

    fn start(mut self) -> std::io::Result<()> {
        debug!("Starting repl\n");

        let stdin = std::io::stdin();

        self.write_displayable(WELCOME_MESSAGE)?;
        self.write_prompt()?;

        for ev in stdin.events() {
            self.handle_event(ev?)?;

            if self.should_exit {
                break;
            }

            self.write_prompt()?;
        }

        Ok(())
    }

    fn lines_to_last_prompt_line(&self) -> u16 {
        self.prev_prompt_lines - 1 - self.prev_cursor_y_in_input as u16
    }

    fn prepare_non_prompt_writing(&mut self) -> std::io::Result<()> {
        if !self.wrote_since_last_prompt {
            self.wrote_since_last_prompt = true;

            // Move to last line of prompt, so the output appears after it
            let move_down_by = self.lines_to_last_prompt_line();
            // Moving down always moves down by at least 1, 0 is not possible
            if move_down_by > 0 {
                write!(self.stdout.0, "{}", termion::cursor::Down(move_down_by))?;
            }
        }
        Ok(())
    }

    fn write_displayable(&mut self, d: impl Display) -> std::io::Result<()> {
        self.prepare_non_prompt_writing()?;
        let string = d.to_string();
        debug!("write_displayable: {}\n", string);
        for line in string.lines() {
            write!(self.stdout.0, "{}", line)?;
            self.write_newline()?;
        }
        Ok(())
    }

    fn write_error(&mut self, e: impl Display) -> std::io::Result<()> {
        self.prepare_non_prompt_writing()?;
        write!(self.stdout.0, "{}", termion::color::Fg(termion::color::Red))?;
        self.write_displayable(e)?;
        write!(
            self.stdout.0,
            "{}",
            termion::color::Fg(termion::color::Reset)
        )
    }

    fn write_newline(&mut self) -> std::io::Result<()> {
        self.prepare_non_prompt_writing()?;
        write_newline(&mut self.stdout.0)
    }

    fn clear(&mut self) -> std::io::Result<()> {
        self.wrote_since_last_prompt = true;
        write!(
            self.stdout.0,
            "{}{}",
            termion::clear::All,
            termion::cursor::Goto(1, 1)
        )
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stdout.0.flush()
    }
}

fn write_newline(mut stdout: impl Write) -> std::io::Result<()> {
    write!(stdout, "\n{}", termion::cursor::Left(65535))
}

pub fn repl() -> std::io::Result<()> {
    Repl::new()?.start()
}
