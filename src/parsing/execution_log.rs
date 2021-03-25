use std::cell::RefCell;

thread_local! {
  static FEEL_EXECUTION_LOG: ExecutionLog = ExecutionLog::new();
}

/// Log operations that yielded a null value, indicating a possible error.
/// 
/// To interact with the per thread global log, 
/// use the static methods log, get, count, and clear.
pub struct ExecutionLog {
  entries: RefCell<Vec<String>>
}

impl ExecutionLog {
  pub fn new() -> Self {
    ExecutionLog {
      entries: RefCell::new(Vec::new())
    }
  }

  /// Log a new message.
  pub fn write<S: Into<String> + Clone>(&self, entry: &S) {
    (*self.entries.borrow_mut()).push(entry.clone().into());
  }

  /// Get all the messages logged so far.
  pub fn read(&self) -> Vec<String> {
    self.entries.borrow().clone()
  }

  /// Clear all log messages.
  pub fn empty(&self) {
    *self.entries.borrow_mut() = Vec::new();
  }

  pub fn len(&self) -> usize {
    self.entries.borrow().len()
  }

  /// Write to the per-thread global execution log.
  pub fn log<S: Into<String> + Clone>(entry: &S) {
    FEEL_EXECUTION_LOG.with(|exec_log| {
      exec_log.write(entry);
    });
  }

  /// Get items in log for current thread.
  pub fn get() -> Vec<String> {
    let mut entries: Vec<String> = Vec::new();
    FEEL_EXECUTION_LOG.with(|exec_log| {
      entries = exec_log.read();
    });
    entries
  }

  /// Count of items in log for current thread.
  pub fn count() -> usize {
    let mut size = 0_usize;
    FEEL_EXECUTION_LOG.with(|exec_log| {
      size = exec_log.len();
    });
    size
  }

  /// Clear log for current thread of all items.
  pub fn clear() {
    FEEL_EXECUTION_LOG.with(|exec_log| {
      exec_log.empty();
    });
  }
}



#[cfg(test)]
mod tests {
  use super::ExecutionLog;
  use std::thread;

  #[test]
  fn test_log() {
    let log = ExecutionLog::new();
    log.write(&"Hello");
    log.write(&"World");
    let messages = log.read();

    let expected = vec!["Hello".to_string(), "World".to_string()];
    assert_eq!(expected, messages);
  }

  #[test]
  fn test_clear() {
    let log = ExecutionLog::new();
    log.write(&"Hello");
    log.write(&"World");
    log.empty();
    assert_eq!(0, log.read().len());
  }

  #[test] 
  fn test_global_log() {
    ExecutionLog::log(&"One");
    thread::spawn(|| {
      ExecutionLog::log(&"Two");
      ExecutionLog::log(&"Three");
      assert_eq!(2, ExecutionLog::count());
    });
    assert_eq!(1, ExecutionLog::count());
  }
}

