use std::{
    cell::OnceCell,
    collections::VecDeque,
    fmt::Display,
    fs::{File, OpenOptions},
    io::Write,
};

use termion::{
    self,
    cursor::DetectCursorPos,
    event::{Event, Key},
    input::TermRead,
    raw::IntoRawMode,
};

use crate::{
    error::ParseError,
    eval::Context,
    eval_str_ctx,
    tokenizer::{Token, TokenKind},
    CalcError,
};

#[cfg(debug_assertions)]
static mut DEBUG_FILE: OnceCell<File> = OnceCell::new();

#[cfg(debug_assertions)]
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

#[derive(Debug)]
struct ReplWriter {
    stdout: std::io::Stdout,
}

impl Write for ReplWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut line_start_idx = 0;
        let mut idx = 0;
        for b in buf {
            if *b == b'\n' {
                let _ = self.stdout.write(&buf[line_start_idx..idx])?;
                write_newline(&mut self.stdout)?;
                line_start_idx = idx + 1;
            }
            idx += 1;
        }

        // Write the rest of the buffer
        let _ = self.stdout.write(&buf[line_start_idx..idx])?;

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stdout.flush()
    }
}

impl ReplWriter {
    fn new() -> ReplWriter {
        ReplWriter {
            stdout: std::io::stdout(),
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

struct Repl {
    ctx: Context,
    input: String,
    line: String,
    pos_in_line: usize,
    input_incomplete: bool,
    ctrl_c_pressed: bool,
    history: History,
    history_index: usize,
    prev_cursor_y_in_input: usize,
    prev_input_lines: u16,
    should_exit: bool,
}

impl Repl {
    fn new() -> Repl {
        let context = Context::builder()
            .stdout(ReplWriter::new())
            .stderr(ReplWriter::new())
            .build();

        Repl {
            ctx: context,
            input: String::new(),
            line: String::new(),
            pos_in_line: 0,
            input_incomplete: false,
            ctrl_c_pressed: false,
            history: History::new(1000),
            history_index: 0,
            prev_cursor_y_in_input: 0,
            prev_input_lines: 1,
            should_exit: false,
        }
    }

    fn handle_event(&mut self, ev: Event) -> std::io::Result<()> {
        let mut stdout = std::io::stdout();

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
                            write_displayable(&mut stdout, "\npress ctrl+c again to exit")?;
                        }
                    } else {
                        self.input.clear();
                        self.line.clear();
                        self.pos_in_line = 0;
                        self.input_incomplete = false;
                        write_newline(&mut stdout)?;
                    }
                }
                Key::Ctrl('l') => {
                    write!(
                        stdout,
                        "{}{}",
                        termion::clear::All,
                        termion::cursor::Goto(1, 1)
                    )?;
                }
                Key::Char('\n') => {
                    write_newline(&mut stdout)?;

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

                    match eval_str_ctx(&self.input, &mut self.ctx) {
                        Ok(result) => {
                            let (cursor_x, _) = stdout.cursor_pos()?;
                            if cursor_x > 1 {
                                write_newline(&mut stdout)?;
                            }

                            write_displayable(&mut stdout, result)?;
                            self.input_incomplete = false;
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
                            self.input_incomplete = true;
                            self.input.push('\n');
                        }
                        Err(err) => {
                            let formatted_err = err.format(&self.input, "<repl>");
                            write_displayable(&mut stdout, formatted_err)?;
                            self.input_incomplete = false;
                            self.input.clear();
                        }
                    };
                }
                Key::Char(c) => {
                    // Currently only ASCII characters are supported
                    if c.len_utf8() > 1 {
                        return Ok(());
                    }

                    self.line.insert(self.pos_in_line, c);
                    self.pos_in_line += 1;
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
                        while self.line.as_bytes()[idx] == b' ' && idx > 0 {
                            idx -= 1;
                        }

                        // Drain word
                        while self.line.as_bytes()[idx] != b' ' && idx > 0 {
                            idx -= 1;
                        }

                        // TODO: This is not correct, fix it
                        if idx > 0 {
                            idx += 1;
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
        let mut stdout = std::io::stdout();

        let current_prefix = if self.input_incomplete {
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
        let input_lines =
            ((prompt_length + term_width.saturating_sub(1) as usize) / term_width as usize) as u16;

        if input_lines > self.prev_input_lines {
            // Make space for another line, but leave cursor in the same y position
            write!(stdout, "\n{}", termion::cursor::Up(1))?;
        }

        let (_, current_cursor_y) = stdout.cursor_pos()?;
        let prompt_start_y = current_cursor_y.saturating_sub(self.prev_cursor_y_in_input as u16);
        let cursor_x = ((current_prefix.len() + self.pos_in_line) % term_width as usize) as u16 + 1;
        let cursor_y = (prompt_start_y as usize + cursor_y_in_input) as u16;

        self.prev_input_lines = input_lines;
        self.prev_cursor_y_in_input = cursor_y_in_input;

        write!(
            stdout,
            "{}{}{}{}{}",
            termion::cursor::Goto(1, prompt_start_y),
            termion::clear::AfterCursor,
            current_prefix,
            self.line,
            termion::cursor::Goto(cursor_x, cursor_y)
        )?;
        stdout.flush()?;
        Ok(())
    }

    fn start(mut self) -> std::io::Result<()> {
        debug!("Starting repl\n");

        let stdin = std::io::stdin();
        let mut stdout = std::io::stdout().into_raw_mode()?;

        write!(stdout, "{}", PREFIX)?;
        stdout.flush()?;

        // TODO: Handle case where something it output by the program and the cursor is not at the
        // end of the line
        for ev in stdin.events() {
            self.handle_event(ev?)?;

            if self.should_exit {
                break;
            }

            self.write_prompt()?;
        }

        Ok(())
    }
}

fn write_displayable(mut stdout: impl Write, d: impl Display) -> std::io::Result<()> {
    let string = d.to_string();
    debug!("write_displayable: {}\n", string);
    for line in string.lines() {
        write!(stdout, "{}", line)?;
        write_newline(&mut stdout)?;
    }
    Ok(())
}

fn write_newline(mut stdout: impl Write) -> std::io::Result<()> {
    write!(stdout, "\n{}", termion::cursor::Left(65535))
}

pub fn repl() -> std::io::Result<()> {
    Repl::new().start()
}
