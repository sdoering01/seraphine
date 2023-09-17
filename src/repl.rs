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
    debug!("Starting repl\n");

    let mut stdout = std::io::stdout().into_raw_mode()?;

    let mut ctx = Context::builder()
        .stdout(ReplWriter::new())
        .stderr(ReplWriter::new())
        .build();

    let stdin = std::io::stdin();

    let mut input = String::new();
    let mut line = String::new();
    let mut pos_in_line = 0;

    let mut input_incomplete = false;

    let prefix = "> ";
    let input_incomplete_prefix = "| ";

    let mut ctrl_c_pressed = false;

    let mut history = History::new(1000);
    let mut history_index = 0;

    let mut prev_cursor_y_in_input = 0;
    let mut prev_input_lines = 1;

    write!(stdout, "{}", prefix)?;
    stdout.flush()?;

    // TODO: Handle case where something it output by the program and the cursor is not at the
    // end of the line

    for ev in stdin.events() {
        if let Event::Key(key) = ev? {
            if !matches!(key, Key::Ctrl('c')) {
                ctrl_c_pressed = false;
            }

            match key {
                Key::Ctrl('d') => {
                    if input.is_empty() && line.is_empty() {
                        break;
                    }
                }
                Key::Ctrl('c') => {
                    if input.is_empty() && line.is_empty() {
                        if ctrl_c_pressed {
                            break;
                        } else {
                            ctrl_c_pressed = true;
                            write_displayable(&mut stdout, "\npress ctrl+c again to exit")?;
                        }
                    } else {
                        input.clear();
                        line.clear();
                        pos_in_line = 0;
                        input_incomplete = false;
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

                    if !input.is_empty() || !line.trim().is_empty() {
                        if !line.trim().is_empty() {
                            history.push(line.clone());
                            history_index = 0;
                        }

                        input.push_str(&line);
                        line.clear();
                        pos_in_line = 0;

                        match eval_str_ctx(&input, &mut ctx) {
                            Ok(result) => {
                                let (cursor_x, _) = stdout.cursor_pos()?;
                                if cursor_x > 1 {
                                    write_newline(&mut stdout)?;
                                }

                                write_displayable(&mut stdout, result)?;
                                input_incomplete = false;
                                input.clear();
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
                                input_incomplete = true;
                                input.push('\n');
                            }
                            Err(err) => {
                                let formatted_err = err.format(&input, "<repl>");
                                write_displayable(&mut stdout, formatted_err)?;
                                input_incomplete = false;
                                input.clear();
                            }
                        };
                    }
                }
                Key::Char(c) => {
                    // Currently only ASCII characters are supported
                    if c.len_utf8() > 1 {
                        continue;
                    }

                    line.insert(pos_in_line, c);
                    pos_in_line += 1;
                }
                Key::Left => {
                    pos_in_line = pos_in_line.saturating_sub(1);
                }
                Key::Right => {
                    if pos_in_line < line.len() {
                        pos_in_line += 1;
                    }
                }
                Key::Up => {
                    if history_index < history.entries_len() {
                        history_index += 1;
                        line = history
                            .get(history_index - 1)
                            .unwrap_or_default()
                            .to_string();
                        pos_in_line = line.len();
                    }
                }
                Key::Down => {
                    if history_index > 0 {
                        history_index -= 1;

                        if history_index > 0 {
                            line = history
                                .get(history_index - 1)
                                .unwrap_or_default()
                                .to_string();
                            pos_in_line = line.len();
                        } else {
                            line.clear();
                            pos_in_line = 0;
                        }
                    }
                }
                Key::Ctrl('w') => {
                    if pos_in_line > 0 {
                        let mut idx = pos_in_line - 1;

                        // Drain whitespaces
                        while line.as_bytes()[idx] == b' ' && idx > 0 {
                            idx -= 1;
                        }

                        // Drain word
                        while line.as_bytes()[idx] != b' ' && idx > 0 {
                            idx -= 1;
                        }

                        if idx > 0 {
                            idx += 1;
                        }

                        line.drain(idx..pos_in_line);
                        pos_in_line = idx;
                    }
                }
                Key::Ctrl('u') => {
                    line.drain(..pos_in_line);
                    pos_in_line = 0;
                }
                Key::Backspace => {
                    if pos_in_line > 0 {
                        line.remove(pos_in_line - 1);
                        pos_in_line -= 1;
                    }
                }
                Key::Delete => {
                    if pos_in_line < line.len() {
                        line.remove(pos_in_line);
                    }
                }
                _ => {}
            }
        }

        let current_prefix = if input_incomplete {
            input_incomplete_prefix
        } else {
            prefix
        };

        let (term_width, _) = termion::terminal_size()?;

        let mut prompt_length = current_prefix.len() + line.len();
        if pos_in_line == line.len() {
            // Account for case where the cursor requires to be placed after the last character
            prompt_length += 1;
        }

        let cursor_y_in_input = (current_prefix.len() + pos_in_line) / term_width as usize;
        // Round up
        let input_lines =
            ((prompt_length + term_width.saturating_sub(1) as usize) / term_width as usize) as u16;

        if input_lines > prev_input_lines {
            // Make space for another line, but leave cursor in the same y position
            write!(stdout, "\n{}", termion::cursor::Up(1))?;
        }

        let (_, current_cursor_y) = stdout.cursor_pos()?;
        let prompt_start_y = current_cursor_y.saturating_sub(prev_cursor_y_in_input as u16);
        let cursor_x = ((current_prefix.len() + pos_in_line) % term_width as usize) as u16 + 1;
        let cursor_y = (prompt_start_y as usize + cursor_y_in_input) as u16;

        prev_input_lines = input_lines;
        prev_cursor_y_in_input = cursor_y_in_input;

        write!(
            stdout,
            "{}{}{}{}{}",
            termion::cursor::Goto(1, prompt_start_y),
            termion::clear::AfterCursor,
            current_prefix,
            line,
            termion::cursor::Goto(cursor_x, cursor_y)
        )?;
        stdout.flush()?;
    }

    Ok(())
}
