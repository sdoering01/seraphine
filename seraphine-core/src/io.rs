use std::{
    io::{self, Read, Write},
    sync::mpsc,
};

pub struct ChannelWriter {
    sender: mpsc::Sender<u8>,
}

impl Write for ChannelWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut sent = 0;
        for b in buf {
            self.sender
                .send(*b)
                .map_err(|_| io::Error::new(io::ErrorKind::Other, "Receiver channel was closed"))?;
            sent += 1;
        }
        Ok(sent)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl ChannelWriter {
    pub fn new(sender: mpsc::Sender<u8>) -> Self {
        Self { sender }
    }
}

pub struct ChannelReader {
    receiver: mpsc::Receiver<u8>,
}

impl Read for ChannelReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut n_bytes = 0;

        // Always read at least one byte in a blocking manner ...
        if !buf.is_empty() {
            buf[0] = self.receiver.recv().map_err(|_| {
                io::Error::new(io::ErrorKind::Other, "All sender channels were closed")
            })?;
            n_bytes += 1;
        }

        // ... then write until the buffer is full or until the channel is empty
        for b in &mut buf[1..] {
            match self.receiver.try_recv() {
                Ok(rb) => {
                    *b = rb;
                    n_bytes += 1;
                }
                Err(mpsc::TryRecvError::Empty) => break,
                Err(mpsc::TryRecvError::Disconnected) => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "All sender channels were closed",
                    ))
                }
            }
        }

        Ok(n_bytes)
    }
}

impl ChannelReader {
    pub fn new(receiver: mpsc::Receiver<u8>) -> Self {
        Self { receiver }
    }

    // TODO: Remove `#[allow(unused)]` once this is used outside of tests
    #[allow(unused)]
    pub fn read_available_to_string(&mut self) -> String {
        let mut buf = Vec::new();
        while let Ok(b) = self.receiver.try_recv() {
            buf.push(b);
        }
        String::from_utf8_lossy(&buf).into_owned()
    }
}

/// Creates a Reader and a Writer that implement [`std::io::Read`] and [`std::io::Write`]
/// respectively. These two objects are linked by a channel, meaning that everything written into
/// the Writer comes out of the Reader.
// TODO: Remove `#[allow(unused)]` once this is used outside of tests
#[allow(unused)]
pub fn create_channel_reader_writer() -> (ChannelReader, ChannelWriter) {
    let (tx, rx) = mpsc::channel();
    (ChannelReader::new(rx), ChannelWriter::new(tx))
}
